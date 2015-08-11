include(mpifx_common.m4)

dnl ************************************************************************
dnl *** mpifx_send
dnl ************************************************************************

define(`_subroutine_mpifx_send', `dnl
dnl $1: subroutine suffix
dnl $2: dummy arguments type
dnl $3: dummy arguments rank specifier ("", (:), (:,:), etc.)
dnl $4: dummy arguments size (1 or len(msg) or size(msg))
dnl $5: corresponding MPI type
!> Sends a message to a given process.
!! \param mycomm  MPI descriptor.
!! \param msg  Msg to be sent.
!! \param dest  Destination process.
!! \param tag  Optional message tag (default: 0).
!! \param error  Optional error handling flag.
!!
subroutine mpifx_send_$1(mycomm, msg, dest, tag, error)
  type(mpifx_comm), intent(in) :: mycomm
  $2, intent(in) :: msg$3
  integer, intent(in) :: dest
  integer, intent(in), optional :: tag
  integer, intent(out), optional :: error

  integer :: tag0, error0

  _handle_inoptflag(tag0, tag, default_tag)
  call mpi_send(msg, $4, $5, dest, tag0, mycomm%id, error0)
  call handle_errorflag(error0, "MPI_SEND in mpifx_send_$1", error)

end subroutine mpifx_send_$1
')
