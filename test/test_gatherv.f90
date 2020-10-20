program test_gatherv
  use libmpifx_module
  implicit none

  type(mpifx_comm) :: mycomm
  integer, parameter :: sp = kind(1.0)
  real(sp), allocatable :: send1(:), send2(:,:)
  real(sp), allocatable :: recv1(:), recv2(:,:)
  real(sp) :: send0
  integer, allocatable :: recvcounts(:)
  integer, allocatable :: displs(:)
  integer :: ii, nrecv
  character(100) :: formstr
  character(*), parameter :: label = "(I2.2,'-',I3.3,'|',1X"

  call mpifx_init()
  call mycomm%init()

  ! R1 -> R1
  if (mycomm%lead) then
    write(*, *) 'Test gather rank=1 -> rank=1'
  end if
  allocate(send1(mycomm%rank+1))
  send1 = real(mycomm%rank+1, sp)
  if (mycomm%lead) then
    ! recv1 size is 1+2+3+...+mycomm%size
    nrecv = mycomm%size*(mycomm%size+1)/2
    allocate(recv1(nrecv))
    allocate(recvcounts(mycomm%size))
    do ii = 1, mycomm%size
      recvcounts(ii) = ii
    end do
  else
    allocate(recv1(0))
  end if
  call mpifx_gatherv(mycomm, send1, recv1, recvcounts)
  if (mycomm%lead) then
    write(*, *) "Recv1 buffer:", recv1
    deallocate(recvcounts)
  end if
  deallocate(recv1)

  ! R2 -> R2
  if (mycomm%lead) then
    write(*, *)
    write(*, *) 'Test gather rank=2 -> rank=2'
  end if
  allocate(send2(10, mycomm%rank+1))
  send2 = real(mycomm%rank + 1, sp)
  if (mycomm%lead) then
    ! recv1 size is 1+2+3+...+mycomm%size
    nrecv = mycomm%size*(mycomm%size+1)/2
    allocate(recv2(10, nrecv))
    recv2 = 0
    allocate(recvcounts(mycomm%size))
    do ii = 1, mycomm%size
      recvcounts(ii) = 10*ii
    end do
  else
    allocate(recv2(0,0))
  end if
  call mpifx_gatherv(mycomm, send2, recv2, recvcounts)
  if (mycomm%lead) then
    write(*, *) "Recv2 buffer:", recv2(:,:)
    deallocate(recvcounts)
  end if
  deallocate(recv2)

  ! R0 -> R1 with specified receive pattern
  if (mycomm%lead) then
    write(*, *)
    write(*, *) 'Test gather scalar -> rank=1'
  end if
  send0 = real(mycomm%rank + 1, sp)
  if (mycomm%lead) then
    nrecv = mycomm%size
    allocate(recv1(nrecv))
    allocate(recvcounts(mycomm%size))
    recvcounts = 1
    allocate(displs(mycomm%size))
    ! set a non trivial displs vector
    do ii = 1, mycomm%size
      displs(ii) = mycomm%size - ii
    end do
  else
    allocate(recv1(0))
  end if
  call mpifx_gatherv(mycomm, send0, recv1, recvcounts, displs)
  if (mycomm%lead) then
    write(*, *) "Recv1 buffer:", recv1
    deallocate(recvcounts)
    deallocate(displs)
  end if
  deallocate(recv1)

  ! R0 -> R1 with specified receive pattern including gaps
  if (mycomm%lead) then
    write(*, *)
    write(*, *) 'Test gather scalar -> rank=1'
  end if
  send0 = real(mycomm%rank + 1, sp)
  if (mycomm%lead) then
    nrecv = mycomm%size
    allocate(recv1(2*nrecv))
    allocate(recvcounts(mycomm%size))
    recvcounts = 1
    allocate(displs(mycomm%size))
    ! set a non trivial displs vector
    do ii = 1, mycomm%size
      displs(ii) = 2*ii-1
    end do
    ! mark untouched elements
    recv1 = -1
  else
    allocate(recv1(0))
  end if
  call mpifx_gatherv(mycomm, send0, recv1, recvcounts, displs)
  if (mycomm%lead) then
    write(*, *) "Recv1 buffer:", recv1
    deallocate(recvcounts)
    deallocate(displs)
  end if
  deallocate(recv1)

  call mpifx_finalize()

end program test_gatherv
