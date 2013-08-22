include(mpifx_common.m4)

dnl ************************************************************************
dnl *** mpifx_allgather
dnl ************************************************************************

define(`_subroutine_mpifx_allgather_dr0',`dnl
dnl
dnl $1: subroutine suffix
dnl $2: send/recv buffer type
dnl $3: send/recv buffer rank specifier ("", (:), (:,:), etc.)
dnl $4: send/recv buffer rank (1, 2, etc.)
dnl $5: corresponding MPI type
dnl
!> Gathers results on all processes (type $1).
!!
!! \param mycomm  MPI communicator.
!! \param send  Quantity to be sent for gathering.
!! \param recv  Received data.
!! \param error  Error code on exit.
!!
subroutine mpifx_allgather_$1(mycomm, send, recv, error)
  type(mpifx_comm), intent(in) :: mycomm
  $2, intent(in) :: send$3
  $2, intent(out) :: recv$3
  integer, intent(out), optional :: error

  integer :: error0

  _assert(size(recv) == size(send) * mycomm%size)
  _assert(size(recv, dim=$4) == size(send, dim=$4) * mycomm%size)

  call mpi_allgather(send, size(send), $5, recv, size(send), &
      & $5, mycomm%id, error0)
  call handle_errorflag(error0, "MPI_ALLGATHER in mpifx_allgather_$1", error)
    
end subroutine mpifx_allgather_$1
')


define(`_subroutine_mpifx_allgather_dr1',`dnl
dnl
dnl $1: subroutine suffix
dnl $2: send/recv buffer type
dnl $3: send buffer rank specifier ("", (:), (:,:), etc.)
dnl $4: send buffer size (1 or size(send))
dnl $5: recv buffer rank specifier ((:), (:,:), etc.)
dnl $6: recv buffers rank (1, 2, etc.)
dnl $7: corresponding MPI type
dnl
!> Gathers results on all processes (type $1).
!!
!! \param mycomm  MPI communicator.
!! \param send  Quantity to be sent for gathering.
!! \param recv  Received data.
!! \param error  Error code on exit.
!!
subroutine mpifx_allgather_$1(mycomm, send, recv, error)
  type(mpifx_comm), intent(in) :: mycomm
  $2, intent(in) :: send$3
  $2, intent(out) :: recv$5
  integer, intent(out), optional :: error

  integer :: error0

  _assert(size(recv) == $4 * mycomm%size)
  _assert(size(recv, dim=$6) == mycomm%size)

  call mpi_allgather(send, $4, $7, recv, $4, &
      & $7, mycomm%id, error0)
  call handle_errorflag(error0, "MPI_ALLGATHER in mpifx_allgather_$1", error)

end subroutine mpifx_allgather_$1

')
