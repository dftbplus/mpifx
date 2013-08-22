include(mpifx_common.m4)

dnl ************************************************************************
dnl *** mpifx_allreduce
dnl ************************************************************************

define(`_subroutine_mpifx_allreduce',`dnl
dnl $1: subroutine suffix
dnl $2: dummy arguments type
dnl $3: dummy arguments rank specifier ("", (:), (:,:), etc.)
dnl $4: dummy arguments size (1 or size(dummyname))
dnl $5: corresponding MPI type
!> Reduces results on all processes (type $1).
!!
!! \param mycomm  MPI communicator.
!! \param operand  Quantity to be reduced.
!! \param result  Contains result on exit.
!! \param operator  Reduction operator
!! \param error  Error code on exit.
!!
subroutine mpifx_allreduce_$1(mycomm, operand, result, operator, error)
  type(mpifx_comm), intent(in) :: mycomm
  $2, intent(in) :: operand$3
  $2, intent(inout) :: result$3
  integer, intent(in) :: operator
  integer, intent(out), optional :: error

  integer :: error0

  call mpi_allreduce(operand, result, $4, $5, operator, mycomm%id, error0)
  call handle_errorflag(error0, "MPI_ALLREDUCE in mpifx_allreduce_$1", error)
    
end subroutine mpifx_allreduce_$1
')
