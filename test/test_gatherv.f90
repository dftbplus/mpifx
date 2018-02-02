program test_gatherv
  use libmpifx_module
  implicit none

  type(mpifx_comm) :: mycomm
  real, allocatable :: send1(:), send2(:,:)
  real, allocatable :: recv1(:), recv2(:,:)
  integer, allocatable :: recvcounts(:)
  integer, allocatable :: displs(:)
  integer :: ii, nrecv  
  character(100) :: formstr
  character(*), parameter :: label = "(I2.2,'-',I3.3,'|',1X"

  call mpifx_init()
  call mycomm%init()

  ! I1 -> I1
  if (mycomm%master) write(*, *) 'Test gather rank=1 -> rank=1'

  allocate(send1(mycomm%rank+1)) 
  send1 = 1.0*(mycomm%rank+1)
  if (mycomm%master) then
    ! recv1 size is 1+2+3+...+mycomm%size 
    nrecv = mycomm%size*(mycomm%size+1)/2
    allocate(recv1(nrecv))
    allocate(recvcounts(mycomm%size))
    do ii = 1, mycomm%size  
      recvcounts(ii) = ii 
    end do 
  else
    allocate(recv1(0))
    allocate(recvcounts(0))
  end if
    

  write(*, *) 'id:',mycomm%rank, "Send1 buffer:", send1(:)

  call mpifx_gatherv(mycomm, send1, recv1, recvcounts)
  
  call mpifx_barrier(mycomm)

  if (mycomm%master) then
    write(*, *) 'id:',mycomm%rank, "Recv1 buffer:", recv1
    deallocate(recvcounts)
  end if
   
  call mpifx_barrier(mycomm)

  ! I2 -> I2
  if (mycomm%master) write(*, *) 'Test gather rank=2 -> rank=2'

  allocate(send2(10, mycomm%rank+1)) 
  send2 = 1.0 * mycomm%rank
  if (mycomm%master) then
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
    

  write(*, *) "id:",mycomm%rank, "Send2 buffer:", send2(:,:)
  
  call mpifx_gatherv(mycomm, send2, recv2, recvcounts)
  
  call mpifx_barrier(mycomm)

  if (mycomm%master) then
    write(*, *) "id:",mycomm%rank, "Recv2 buffer:", recv2(:,:)
  end if


  call mpifx_finalize()
  
end program test_gatherv
