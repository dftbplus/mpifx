include(mpifx_common.m4)

dnl ************************************************************************
dnl *** mpifx_allgatherv
dnl ************************************************************************

define(`_subroutine_mpifx_allgatherv_dr0',`dnl
dnl
dnl $1: subroutine suffix
dnl $2: send/recv buffer type
dnl $3: send/recv buffer rank specifier ("", (:), (:,:), etc.)
dnl $4: send/recv buffer rank (1, 2, etc.)
dnl $5: corresponding MPI type
dnl
!> Gathers results of variable length on all processes (type $1).
!!
!! \param mycomm  MPI communicator.
!! \param send  Quantity to be sent for gathering.
!! \param recv  Received data
!! \param recvcounts Counts of received data from each process
!! \param displs Entry i specifies where to place data from process rank i-1
!!               (default: computed from recvcounts assuming order with rank)
!! \param error  Error code on exit.
!!
subroutine mpifx_allgatherv_$1(mycomm, send, recv, recvcounts, displs, error)
  type(mpifx_comm), intent(in) :: mycomm
  $2, intent(in) :: send$3
  $2, intent(out) :: recv$3
  integer, intent(in) :: recvcounts(:)
  integer, intent(in), optional :: displs(:)
  integer, intent(out), optional :: error

  integer :: error0, ii
  integer, allocatable :: displs0(:)


  _assert(size(recv) == sum(recvcounts))
  allocate(displs0(mycomm%size))
  if (present(displs)) then
    _assert(size(displs) == mycomm%size)
    displs0 = displs
  else
    displs0(1) = 0
    do ii = 2, mycomm%size
      displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
    end do
  end if

  call mpi_allgatherv(send, size(send), $5, recv, recvcounts, displs0, &
      & $5, mycomm%id, error0)

  call handle_errorflag(error0, "MPI_ALLGATHERV in mpifx_allgatherv_$1", error)

end subroutine mpifx_allgatherv_$1
')


define(`_subroutine_mpifx_allgatherv_dr1',`dnl
dnl
dnl $1: subroutine suffix
dnl $2: send/recv buffer type
dnl $3: send buffer rank specifier ("", (:), (:,:), etc.)
dnl $4: send buffer size (1 or size(send))
dnl $5: recv buffer rank specifier ((:), (:,:), etc.)
dnl $6: recv buffers rank (1, 2, etc.)
dnl $7: corresponding MPI type
dnl
!> Gathers results on one process (type $1).
!!
!! \param mycomm  MPI communicator.
!! \param send  Quantity to be sent for gathering.
!! \param recv  Received data on receive node (indefined on other nodes)
!! \param recvcounts Counts of received data from each process
!! \param displs Entry i specifies where to place data from process rank i-1
!!               (default: computed from recvcounts assuming order with rank)
!! \param error  Error code on exit.
!!
subroutine mpifx_allgatherv_$1(mycomm, send, recv, recvcounts, displs, error)
  type(mpifx_comm), intent(in) :: mycomm
  $2, intent(in) :: send$3
  $2, intent(out) :: recv$5
  integer, intent(in) :: recvcounts(:)
  integer, intent(in), optional :: displs(:)
  integer, intent(out), optional :: error

  integer :: ii, error0
  integer, allocatable :: displs0(:)

  _assert(size(recv) == sum(recvcounts))
  _assert(size(recv, dim=$6) == mycomm%size)
  allocate(displs0(mycomm%size))
  if (present(displs)) then
    _assert(size(displs) == mycomm%size)
    displs0 = displs
  else
    displs0(1) = 0
    do ii = 2, mycomm%size
      displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
    end do
  end if

  call mpi_allgatherv(send, $4, $7, recv, recvcounts, displs0, &
       & $7,  mycomm%id, error0)

  call handle_errorflag(error0, "MPI_ALLGATHERV in mpifx_allgatherv_$1", error)

end subroutine mpifx_allgatherv_$1
')
