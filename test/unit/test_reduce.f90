program test_reduce
  use libmpifx_module
  implicit none

  integer, parameter :: dp = kind(1.0d0)

  type(mpifx_comm) :: mycomm
  integer :: vali0, resvali0
  real(dp) :: valr(3), resvalr(3)

  call mpifx_init()
  call mycomm%init()

  ! Reduction of a scalarw
  vali0 = mycomm%rank * 2
  write(*, "(I2.2,'-',I3.3,'|',1X,A,I0)") 1, mycomm%rank, &
      & "Value to be operated on:", vali0
  call mpifx_reduce(mycomm, vali0, resvali0, MPI_SUM)
  write(*, "(I2.2,'-',I3.3,'|',1X,A,I0)") 2, mycomm%rank, &
      & "Obtained result (sum):", resvali0

  ! Reduction of an array
  valr(:) = [ real(mycomm%rank + 1, dp) * 1.2, &
      & real(mycomm%rank + 1, dp) * 4.3, real(mycomm%rank + 1, dp) * 3.8 ]
  resvalr(:) = 0.0_dp
  write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 3, mycomm%rank, &
      & "Value to be operated on:", valr(:)
  call mpifx_reduce(mycomm, valr, resvalr, MPI_PROD)
  write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 4, mycomm%rank, &
      & "Obtained result (prod):", resvalr(:)

  ! In place summation
  resvalr(:) = [ real(mycomm%rank + 1, dp) * 1.2, &
      & real(mycomm%rank + 1, dp) * 4.3, real(mycomm%rank + 1, dp) * 3.8 ]
  write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 5, mycomm%rank, &
      & "Value to be operated on:", resvalr(:)
  call mpifx_reduceip(mycomm, resvalr, MPI_SUM)
  write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 6, mycomm%rank, &
      & "Obtained result (sum):", resvalr(:)
  
  call mpifx_finalize()
  
end program test_reduce
