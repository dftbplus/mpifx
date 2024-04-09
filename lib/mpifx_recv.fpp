#:include 'mpifx.fypp'
#:set TYPES = ALL_TYPES
#:set RANKS = range(MAX_RANK + 1)

!> Contains wrapper for \c MPI_RECV
module mpifx_recv_module
  use mpi_f08
  use mpifx_comm_module, only : mpifx_comm
  use mpifx_helper_module, only : dp, sp
  implicit none
  private

  public :: mpifx_recv


  !> Receives a message from a given node.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second argument. The second argument can be of
  !! type integer (i), real (s), double precision (d), complex (c),
  !! double complex (z), logical (b) and character (h). Its rank can vary from
  !! zero (scalar) up to the maximum rank.
  !!
  !! \see MPI documentation (\c MPI_RECV)
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
  interface mpifx_recv
#:for TYPE in TYPES
  #:for RANK in RANKS
    module procedure mpifx_recv_mpi_f08${TYPE_ABBREVS[TYPE]}$${RANK}$
    module procedure mpifx_recv_mpi${TYPE_ABBREVS[TYPE]}$${RANK}$
  #:endfor
#:endfor
  end interface mpifx_recv

contains

#:def mpifx_recv_template(SUFFIX, TYPE, MPITYPE, RANK, HASLENGTH)

  #:assert RANK >= 0

  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array as integer.
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_mpi_f08${SUFFIX}$(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    ${TYPE}$, intent(out) :: msg${RANKSUFFIX(RANK)}$
    integer, intent(in), optional :: source, tag
    type(mpi_status), intent(out), optional :: status
    integer, intent(out), optional :: error

    integer :: source0, tag0, error0
    type(mpi_status) :: status0

    call getoptarg(MPI_ANY_TAG, tag0, tag)
    call getoptarg(MPI_ANY_SOURCE, source0, source)

    #:set SIZE = '1' if RANK == 0 else 'size(msg)'
    #:set COUNT = ('len(msg) * ' + SIZE if HASLENGTH else SIZE)

    call mpi_recv(msg, ${COUNT}$, ${MPITYPE}$, source0, tag0, mycomm%comm, status0, error0)
    call handle_errorflag(error0, "MPI_RECV in mpifx_recv_${SUFFIX}$", error)

    if (present(status)) status = status0

  end subroutine mpifx_recv_mpi_f08${SUFFIX}$

  !> Receives a message from a given process.
  !! \param mycomm  MPI descriptor.
  !! \param msg  Msg to be received.
  !! \param source  Optional source process (default: MPI_ANY_SOURCE)
  !! \param tag  Optional message tag (default: MPI_ANY_TAG).
  !! \param status  Optional status array as type(mpi_status).
  !! \param error  Optional error handling flag.
  !!
  subroutine mpifx_recv_mpi${SUFFIX}$(mycomm, msg, source, tag, status, error)
    type(mpifx_comm), intent(in) :: mycomm
    ${TYPE}$, intent(out) :: msg${RANKSUFFIX(RANK)}$
    integer, intent(in) :: source, tag
    integer, intent(out) :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: error

    type(mpi_status) :: newstatus

    call mpifx_recv(mycomm, msg, source, tag, newstatus, error)
    call mpi_status_f082f(newstatus, status)

  end subroutine mpifx_recv_mpi${SUFFIX}$

#:enddef mpifx_recv_template


#:for TYPE in TYPES
  #:for RANK in RANKS

    #:set FTYPE = FORTRAN_TYPES[TYPE]
    #:set MPITYPE = MPI_TYPES[TYPE]
    #:set HASLENGTH = HAS_LENGTH[TYPE]
    #:set SUFFIX = TYPE_ABBREVS[TYPE] + str(RANK)

    $:mpifx_recv_template(SUFFIX, FTYPE, MPITYPE, RANK, HASLENGTH)

  #:endfor
#:endfor

end module mpifx_recv_module
