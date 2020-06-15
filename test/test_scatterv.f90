program test_scatterv
  use libmpifx_module
  implicit none

  type(mpifx_comm) :: mycomm
  integer, allocatable :: send1(:), send2(:,:)
  integer :: recv0
  integer, allocatable :: recv1(:), sendcount(:), displs(:)
  character(100) :: formstr
  character(*), parameter :: label = "(I2.2,'-',I3.3,'|',1X"
  integer :: ii

  call mpifx_init()
  call mycomm%init()

  ! I1 -> I0
  if (mycomm%lead) then
    allocate(send1(mycomm%size))
    allocate(sendcount(mycomm%size))
    send1(:) = [ (ii, ii = 1, size(send1)) ]
    sendcount(:) = 1
    write(formstr, "(A,I0,A)") "A,", size(send1), "(1X,I0))"
    write(*, label // formstr) 1, mycomm%rank, "Send1 buffer:", send1
  else
    allocate(send1(0))
    allocate(sendcount(0))
  end if
  recv0 = 0
  call mpifx_scatterv(mycomm, send1, sendcount, recv0)
  write(formstr, "(A,I0,A)") "A,", 1, "(1X,I0))"
  write(*, label // formstr) 2, mycomm%rank, "Recv0 buffer:", recv0

  ! I1 -> I1
  if (mycomm%lead) then
    deallocate(send1)
    allocate(send1(2 * mycomm%size))
    sendcount(:) = 2
    send1(:) = [ (ii, ii = 1, size(send1)) ]
    write(formstr, "(A,I0,A)") "A,", size(send1), "(1X,I0))"
    write(*, label // formstr) 3, mycomm%rank, "Send1 buffer:", send1
  end if
  allocate(recv1(2))
  recv1(:) = 0
  call mpifx_scatterv(mycomm, send1, sendcount, recv1)
  write(formstr, "(A,I0,A)") "A,", size(recv1), "(1X,I0))"
  write(*, label // formstr) 4, mycomm%rank, "Recv1 buffer:", recv1

  ! I2 -> I1
  if (mycomm%lead) then
    allocate(send2(2, mycomm%size))
    sendcount(:) = 2
    send2(:,:) = reshape(send1,  [ 2, mycomm%size ])
    write(formstr, "(A,I0,A)") "A,", size(send2), "(1X,I0))"
    write(*, label // formstr) 5, mycomm%rank, &
        & "Send2 buffer:", send2
  else
    allocate(send2(0,0))
  end if
  recv1(:) = 0
  call mpifx_scatterv(mycomm, send2, sendcount, recv1)
  write(formstr, "(A,I0,A)") "A,", size(recv1), "(1X,I0))"
  write(*, label // formstr) 6, mycomm%rank, &
      & "Recv1 buffer:", recv1

  ! I1 -> I1
  if (mycomm%lead) then
    deallocate(send1)
    allocate(send1(2 * mycomm%size))
    send1(:) = [ (ii, ii = 1, size(send1)) ]
    sendcount(:) = 1
    allocate(displs(mycomm%size))
    displs(:) = [ (ii, ii = 1, size(send1), 2) ]
    write(formstr, "(A,I0,A)") "A,", size(send1), "(1X,I0))"
    write(*, label // formstr) 7, mycomm%rank, "Send1 buffer:", send1
  end if
  deallocate(recv1)
  allocate(recv1(1))
  recv1(:) = 0
  call mpifx_scatterv(mycomm, send1, sendcount, recv1, displs=displs)
  write(formstr, "(A,I0,A)") "A,", size(recv1), "(1X,I0))"
  write(*, label // formstr) 8, mycomm%rank, "Recv1 buffer:", recv1

  call mpifx_finalize()
  
end program test_scatterv
