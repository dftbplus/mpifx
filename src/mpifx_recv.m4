include(mpifx_common.m4)

dnl ************************************************************************
dnl *** mpifx_recv
dnl ************************************************************************

define(`_subroutine_mpifx_recv', `dnl
dnl $1: subroutien suffix
dnl $2: dummy arguments type
dnl $3: dummy arguments rank specifier ("", (:), (:,:), etc.)
dnl $4: dummy arguments size (1 or size(dummyname))
dnl $5: corresponding MPI type
!> Receives a message from a given process.
!! \param mycomm  MPI descriptor.
!! \param msg  Msg to be received.
!! \param source  Optional source process (default: MPI_ANY_SOURCE)
!! \param tag  Optional message tag (default: MPI_ANY_TAG).
!! \param status  Optional status array.
!! \param error  Optional error handling flag.
!!
subroutine mpifx_recv_$1(mycomm, msg, source, tag, status, error)
  type(mpifx_comm), intent(in) :: mycomm
  $2, intent(out) :: msg$3
  integer, intent(in), optional :: source, tag
  integer, intent(out), optional :: status(MPI_STATUS_SIZE)
  integer, intent(out), optional :: error

  integer :: source0, tag0, error0
  integer :: status0(MPI_STATUS_SIZE)
  
  _handle_inoptflag(tag0, tag, MPI_ANY_TAG)
  _handle_inoptflag(source0, source, MPI_ANY_SOURCE)
  call mpi_recv(msg, $4, $5, source0, tag0, mycomm%id, status0, &
      & error0)
  call handle_errorflag(error0, "MPI_RECV in mpifx_recv_$1", error)
  _handle_outoptflag(status, status0)

end subroutine mpifx_recv_$1
')
