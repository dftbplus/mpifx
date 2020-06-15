#:include 'mpifx.fypp'
#:set TYPES = ALL_TYPES
#:set RANKS = range(MAX_RANK + 1)

!> Contains wrapper for \c MPI_BCAST.
module mpifx_bcast_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_bcast

  !> Broadcasts an MPI message to all nodes.
  !!
  !! \details All functions have the same argument list only differing in the type and rank of the
  !! second argument. The second argument can be of type integer, real, double precision, complex,
  !! double complex, logical and character. Its rank can vary from zero (scalar) up to the maximum
  !! rank.
  !!
  !! \see MPI documentation (\c MPI_BCAST)
  !!
  !! Example:
  !!
  !!     program test_bcast
  !!       use libmpifx_module
  !!
  !!       type(mpifx) :: mycomm
  !!       integer :: buffer(3)
  !!
  !!       call mycomm%init()
  !!       if (mycomm%lead) then
  !!         buffer(:) = [ 1, 2, 3 ]
  !!       end if
  !!       call mpifx_bcast(mycomm, buffer)
  !!       print "(A,I2.2,A,3I5)", "BUFFER:", mycomm%rank, ":", buffer
  !!       call mycomm%destruct()
  !!
  !!     end program test_bcast
  !!
  interface mpifx_bcast
#:for TYPE in TYPES
  #:for RANK in RANKS
    module procedure mpifx_bcast_${TYPE_ABBREVS[TYPE]}$${RANK}$
  #:endfor
#:endfor
  end interface

contains

#:def mpifx_bcast_template(SUFFIX, TYPE, MPITYPE, RANK, HASLENGTH)

  #:assert RANK >= 0

  !> Broadcasts an MPI message to all nodes (type ${SUFFIX}$).
  !!
  subroutine mpifx_bcast_${SUFFIX}$(mycomm, msg, root, error)

    !> MPI descriptor
    type(mpifx_comm), intent(in) :: mycomm

    !> Msg to be broadcasted on root and received on non-root nodes.
    ${TYPE}$ :: msg${RANKSUFFIX(RANK)}$

    !> Root node for the broadcast (default: mycomm%leadrank).
    integer, intent(in), optional :: root

    !> Optional error handling flag.
    integer, intent(out), optional :: error

    integer :: root0, error0

    #:set SIZE = '1' if RANK == 0 else 'size(msg)'
    #:set COUNT = ('len(msg) * ' + SIZE if HASLENGTH else SIZE)

    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_bcast(msg, ${COUNT}$, ${MPITYPE}$, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_${SUFFIX}$", error)

  end subroutine mpifx_bcast_${SUFFIX}$

#:enddef mpifx_bcast_template

  
#:for TYPE in TYPES
  #:for RANK in RANKS

    #:set SUFFIX = TYPE_ABBREVS[TYPE] + str(RANK)
    #:set FTYPE = FORTRAN_TYPES[TYPE]
    #:set MPITYPE = MPI_TYPES[TYPE]
    #:set HASLENGTH = HAS_LENGTH[TYPE]

    $:mpifx_bcast_template(SUFFIX, FTYPE, MPITYPE, RANK, HASLENGTH)

  #:endfor
#:endfor

end module mpifx_bcast_module
