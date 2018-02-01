include(mpifx_common.m4)

dnl ************************************************************************
dnl *** mpifx_gatherv
dnl ************************************************************************

define(`_subroutine_mpifx_gatherv_dr0',`dnl
dnl
dnl $1: subroutine suffix
dnl $2: send/recv buffer type
dnl $3: send/recv buffer rank specifier ("", (:), (:,:), etc.)
dnl $4: send/recv buffer rank (1, 2, etc.)
dnl $5: corresponding MPI type
dnl
!> Gathers results of variable length on one process (type $1).
!!
!! \param mycomm  MPI communicator.
!! \param send  Quantity to be sent for gathering.
!! \param recv  Received data on receive node (undefined on other nodes)
!! \param recvcounts Counts of received data from each process
!! \param root  Root process for the result (default: mycomm%masterrank)
!! \param error  Error code on exit.
!!
subroutine mpifx_gatherv_$1(mycomm, send, recv, recvcounts, root, error)
  type(mpifx_comm), intent(in) :: mycomm
  $2, intent(in) :: send$3
  $2, intent(out) :: recv$3
  integer, intent(in) :: recvcounts(:)
  integer, intent(in), optional :: root
  integer, intent(out), optional :: error

  integer :: root0, error0, ii
  integer, allocatable :: displs(:)

  _assert(.not. mycomm%master .or. size(recv) == sum(recvcounts))

  _handle_inoptflag(root0, root, mycomm%masterrank)

  allocate(displs(mycomm%size))
  displs(1) = 0
  do ii = 2, mycomm%size
    displs(ii) = displs(ii-1) + recvcounts(ii-1)
  end do
  call mpi_gatherv(send, size(send), $5, recv, recvcounts, displs, &
      & $5, root0, mycomm%id, error0)
  call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_$1", error)
    
end subroutine mpifx_gatherv_$1
')

