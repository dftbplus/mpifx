include(common.m4)

dnl ************************************************************************
dnl *** mpifx_reduce
dnl ************************************************************************

define(`_subroutine_mpifx_reduce',`dnl
dnl $1: subroutine suffix
dnl $2: dummy arguments type
dnl $3: dummy arguments rank specifier ("", (:), (:,:), etc.)
dnl $4: dummy arguments size (1 or size(dummyname))
dnl $5: corresponding MPI type
!> Reduces results on one process (type $1).
!!
!! \param mycomm  MPI communicator.
!! \param operand  Quantity to be reduced.
!! \param result  Contains result on exit.
!! \param operator  Reduction operator
!! \param root  Root process for the result (default: mycomm%masterrank)
!! \param error  Error code on exit.
!!
subroutine mpifx_reduce_$1(mycomm, operand, result, operator, root, error)
  type(mpifx_comm), intent(in) :: mycomm
  $2, intent(in) :: operand$3
  $2, intent(inout) :: result$3
  integer, intent(in) :: operator
  integer, intent(in), optional :: root
  integer, intent(out), optional :: error

  integer :: root0, error0

  _handle_inoptflag(root0, root, mycomm%masterrank)
  call mpi_reduce(operand, result, $4, $5, operator, root0, mycomm%id, error0)
  call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_$1", error)
    
end subroutine mpifx_reduce_$1
')
