#:include 'mpifx.fypp'
#:set RANKS = range(MAX_RANK + 1)
#:set TYPES = ALL_TYPES

!> Contains wrapper for \c MPI_SEND
module mpifx_send_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_send


  !> Sends a message to a given node.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second argument. The second argument can be of
  !! type integer (i), real (s), double precision (d), complex (c), 
  !! double complex (z), logical (b) and character (h). Its rank can vary from
  !! zero (scalar) up to the maximum rank.
  !!
  !! \see MPI documentation (\c MPI_SEND)
  !!
  !! Example:
  !!
  !!     program hello
  !!     use libmpifx_module
  !!     implicit none
  !!
  !!     character(100) :: msg
  !!     type(mpifx) :: mycomm
  !!     integer :: source
  !!
  !!     call mpifx_init()
  !!     call mycomm%init()
  !!     if (.not. mycomm%lead) then
  !!       write(msg, "(A,I0,A)") "Hello from process ", mycomm%rank, "!"
  !!       call mpifx_send(mycomm, msg, mycomm%leadrank)
  !!     else
  !!       write(*, "(A)") "Lead node:"
  !!       do source = 1, mycomm%size - 1
  !!         call mpifx_recv(mycomm, msg, source)
  !!         write(*,"(A,A)") "Message received: ", trim(msg)
  !!       end do
  !!     end if
  !!     call mpifx_finalize()
  !!
  !!   end program hello
  !!
  interface mpifx_send
#:for TYPE in TYPES
  #:for RANK in RANKS
    module procedure mpifx_send_${TYPE_ABBREVS[TYPE]}$${RANK}$
  #:endfor
#:endfor
  end interface mpifx_send

contains

#:def mpifx_send_template(SUFFIX, TYPE, MPITYPE, RANK, HASLENGTH)

  !> Sends a message to a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be sent.
  !! \param dest  Destination process.
  !! \param tag  Optional message tag (default: 0).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_send_${SUFFIX}$(mycomm, msg, dest, tag, error)
    type(mpifx_comm), intent(in) :: mycomm
    ${TYPE}$, intent(in) :: msg${RANKSUFFIX(RANK)}$
    integer, intent(in) :: dest
    integer, intent(in), optional :: tag
    integer, intent(out), optional :: error

    integer :: tag0, error0

    #:set SIZE = '1' if RANK == 0 else 'size(msg)'
    #:set COUNT = ('len(msg) * ' + SIZE if HASLENGTH else SIZE)

    call getoptarg(default_tag, tag0, tag)
    call mpi_send(msg, ${COUNT}$, ${MPITYPE}$, dest, tag0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SEND in mpifx_send_${SUFFIX}$", error)

  end subroutine mpifx_send_${SUFFIX}$

#:enddef mpifx_send_template


#:for TYPE in TYPES
  #:for RANK in RANKS

    #:set FTYPE = FORTRAN_TYPES[TYPE]
    #:set MPITYPE = MPI_TYPES[TYPE]
    #:set HASLENGTH = HAS_LENGTH[TYPE]
    #:set SUFFIX = TYPE_ABBREVS[TYPE] + str(RANK)

    $:mpifx_send_template(SUFFIX, FTYPE, MPITYPE, RANK, HASLENGTH)

  #:endfor
#:endfor



end module mpifx_send_module
