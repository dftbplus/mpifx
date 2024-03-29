!> Test various patterns of allgatherv
program test_allgatherv
  use libmpifx_module
  use testhelper
  implicit none

  type(mpifx_comm) :: mycomm
  integer, parameter :: sp = kind(1.0)
  real(sp), allocatable :: send1(:), send2(:,:)
  real(sp), allocatable :: recv1(:), recv2(:,:)
  real(sp) :: send0
  integer, allocatable :: recvcounts(:)
  integer, allocatable :: displs(:)
  integer :: ii, nrecv, nCol
  character(100) :: formstr
  character(*), parameter :: label = "(I2.2,'-',I3.3,'|',1X"
  logical :: tPassed
  integer :: iCount

  call mpifx_init()
  call mycomm%init()

  ! R1 -> R1
  if (mycomm%rank == mycomm%size - 1) then
    write(*, *) 'Test gather rank=1 -> rank=1'
  end if
  allocate(send1(mycomm%rank+1))
  send1 = real(mycomm%rank+1, sp)
  ! recv1 size is 1+2+3+...+mycomm%size
  nrecv = mycomm%size*(mycomm%size+1)/2
  allocate(recv1(nrecv))
  allocate(recvcounts(mycomm%size))
  do ii = 1, mycomm%size
    recvcounts(ii) = ii
  end do
  call mpifx_allgatherv(mycomm, send1, recv1, recvcounts)
  if (mycomm%rank == mycomm%size - 1) then
    write(*, *) "Recv1 buffer:", recv1
  end if
  ! test what has been gathered
  iCount = (2*mycomm%size**3+3*mycomm%size**2+mycomm%size)/6
  if (nint(sum(recv1)) /= iCount) then
    tPassed = .false.
  else
    tPassed = .true.
  end if
  tPassed = tPassed .and. (abs(sum(recv1)-nint(sum(recv1))) < epsilon(1.0_sp))
  call testReturn(mycomm, tPassed)
  deallocate(recvcounts)
  deallocate(recv1)

  ! R2 -> R2
  if (mycomm%rank == mycomm%size - 1) then
    write(*, *)
    write(*, *) 'Test gather rank=2 -> rank=2'
  end if
  nCol = 5
  allocate(send2(nCol, mycomm%rank+1))
  send2 = real(mycomm%rank + 1, sp)
  ! recv1 size is 1+2+3+...+mycomm%size
  nrecv = mycomm%size*(mycomm%size+1)/2
  allocate(recv2(nCol, nrecv))
  recv2 = 0
  allocate(recvcounts(mycomm%size))
  do ii = 1, mycomm%size
    recvcounts(ii) = nCol*ii
  end do
  call mpifx_allgatherv(mycomm, send2, recv2, recvcounts)
  if (mycomm%rank == mycomm%size - 1) then
    write(*, *) "Recv2 buffer:", shape(recv2)
    do ii = 1, nrecv
      write(*,*)recv2(:,ii)
    end do
  end if
  iCount = 5*mycomm%size*(mycomm%size+1)*(2*mycomm%size+1)/6
  if (nint(sum(recv2)) /= iCount) then
    tPassed = .false.
  else
    tPassed = .true.
  end if
  tPassed = tPassed .and. (abs(sum(recv2)-nint(sum(recv2))) < epsilon(1.0_sp))
  call testReturn(mycomm, tPassed)
  deallocate(recvcounts)


  ! R0 -> R1 with specified receive pattern
  if (mycomm%rank == mycomm%size - 1) then
    write(*, *)
    write(*, *) 'Test gather scalar -> rank=1'
  end if
  send0 = real(mycomm%rank + 1, sp)
  nrecv = mycomm%size
  allocate(recv1(nrecv))
  allocate(recvcounts(mycomm%size))
  recvcounts = 1
  allocate(displs(mycomm%size))
  ! set a non trivial displs vector
  do ii = 1, mycomm%size
    displs(ii) = mycomm%size - ii
  end do
  call mpifx_allgatherv(mycomm, send0, recv1, recvcounts, displs)
  if (mycomm%rank == mycomm%size - 1) then
    write(*, *) "Recv1 buffer:", recv1
  end if
  ! test what has been gathered
  if (nint(sum(recv1)) /= (mycomm%size*(mycomm%size+1))/2) then
    tPassed = .false.
  else
    tPassed = .true.
  end if
  tPassed = tPassed .and. (abs(sum(recv1)-nint(sum(recv1))) < epsilon(1.0_sp))
  call testReturn(mycomm, tPassed)

  call mpifx_finalize()

end program test_allgatherv
