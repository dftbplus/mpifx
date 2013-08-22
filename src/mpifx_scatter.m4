include(mpifx_common.m4)

dnl ************************************************************************
dnl *** mpifx_scatter
dnl ************************************************************************

define(`_subroutine_mpifx_scatter_dr0',`dnl
dnl
dnl $1: subroutine suffix
dnl $2: send/recv buffer type
dnl $3: send/recv buffer rank specifier ("", (:), (:,:), etc.)
dnl $4: send/recv buffer rank (1, 2, etc.)
dnl $5: corresponding MPI type
dnl
!> Scatters object from one process (type $1).
!!
!! \param mycomm  MPI communicator.
!! \param send  Quantity to be sent for scattering.
!! \param recv  Received data on receive node (undefined on other nodes)
!! \param root  Root process for the result (default: mycomm%masterrank)
!! \param error  Error code on exit.
!!
subroutine mpifx_scatter_$1(mycomm, send, recv, root, error)
  type(mpifx_comm), intent(in) :: mycomm
  $2, intent(in) :: send$3
  $2, intent(out) :: recv$3
  integer, intent(in), optional :: root
  integer, intent(out), optional :: error

  integer :: root0, error0

  _assert(.not. mycomm%master .or. size(send) == size(recv) * mycomm%size)
  _assert(.not. mycomm%master .or. &
      & size(send, dim=$4) == size(recv, dim=$4) * mycomm%size)

  _handle_inoptflag(root0, root, mycomm%masterrank)
  call mpi_scatter(send, size(recv), $5, recv, size(recv), &
      & $5, root0, mycomm%id, error0)
  call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_$1", error)
    
end subroutine mpifx_scatter_$1
')


define(`_subroutine_mpifx_scatter_dr1',`dnl
dnl
dnl $1: subroutine suffix
dnl $2: send/recv buffer type
dnl $3: recv buffer rank specifier ("", (:), (:,:), etc.)
dnl $4: recv buffer size (1 or size(recv))
dnl $5: send buffer rank specifier ((:), (:,:), etc.)
dnl $6: send buffer rank (1, 2, etc.)
dnl $7: corresponding MPI type
dnl
!> Scatters results on one process (type $1).
!!
!! \param mycomm  MPI communicator.
!! \param send  Quantity to be sent for scattering.
!! \param recv  Received data on receive node (indefined on other nodes)
!! \param root  Root process for the result (default: mycomm%masterrank)
!! \param error  Error code on exit.
!!
subroutine mpifx_scatter_$1(mycomm, send, recv, root, error)
  type(mpifx_comm), intent(in) :: mycomm
  $2, intent(in) :: send$5
  $2, intent(out) :: recv$3
  integer, intent(in), optional :: root
  integer, intent(out), optional :: error

  integer :: root0, error0

  _assert(.not. mycomm%master .or. size(send) == $4 * mycomm%size)
  _assert(.not. mycomm%master .or. size(send, dim=$6) == mycomm%size)

  _handle_inoptflag(root0, root, mycomm%masterrank)
  call mpi_scatter(send, $4, $7, recv, $4, &
      & $7, root0, mycomm%id, error0)
  call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_$1", error)

end subroutine mpifx_scatter_$1

')
