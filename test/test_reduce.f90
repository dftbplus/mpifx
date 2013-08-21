program test_reduce
  use libmpifx_module
  implicit none

  integer, parameter :: dp = kind(1.0d0)

  type(mpifx_comm) :: mycomm
  integer :: vali0, resvali0
  real(dp) :: valr(3), resvalr(3)

  call mpifx_init()
  call mycomm%init()

  vali0 = mycomm%iproc * 2
  write(*, "(I2.2,'-',I3.3,'|',1X,A,I0)") 1, mycomm%iproc, &
      & "Value to be operated on:", vali0
  call mpifx_reduce(mycomm, vali0, resvali0, MPI_SUM)
  write(*, "(I2.2,'-',I3.3,'|',1X,A,I0)") 2, mycomm%iproc, &
      & "Obtained result (sum):", resvali0
  valr(:) = [ real(mycomm%iproc + 1, dp) * 1.2, &
      & real(mycomm%iproc + 1, dp) * 4.3, real(mycomm%iproc + 1, dp) * 3.8 ]
  write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 3, mycomm%iproc, &
      & "Value to be operated on:", valr(:)
  call mpifx_reduce(mycomm, valr, resvalr, MPI_PROD)
  write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 4, mycomm%iproc, &
      & "Obtained result (prod):", resvalr(:)
  call mpifx_finalize()
  
end program test_reduce
