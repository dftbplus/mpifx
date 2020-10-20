program test_allgather
  use libmpifx_module
  implicit none

  type(mpifx_comm) :: mycomm
  integer :: send0
  integer, allocatable :: send1(:)
  integer, allocatable :: recv1(:), recv2(:,:)
  character(100) :: formstr
  character(*), parameter :: label = "(I2.2,'-',I3.3,'|',1X"

  call mpifx_init()
  call mycomm%init()

  ! I0 -> I1
  send0 = mycomm%rank * 2
  allocate(recv1(1 * mycomm%size))
  recv1(:) = 0
  write(*, label // ",A,1X,I0)") 1, mycomm%rank, &
      & "Send0 buffer:", send0
  call mpifx_allgather(mycomm, send0, recv1)
  write(formstr, "(A,I0,A)") "A,", size(recv1), "(1X,I0))"
  write(*, label // formstr) 2, mycomm%rank, &
      & "Recv1 buffer:", recv1(:)
  deallocate(recv1)

  ! I1 -> I1
  allocate(send1(2))
  allocate(recv1(size(send1) * mycomm%size))
  recv1(:) = 0
  send1(:) = [ mycomm%rank, mycomm%rank + 1 ]
  write(formstr, "(A,I0,A)") "A,", size(send1), "(1X,I0))"
  write(*, label // formstr) 3, mycomm%rank, &
      & "Send1 buffer:", send1(:)
  call mpifx_allgather(mycomm, send1, recv1)
  write(formstr, "(A,I0,A)") "A,", size(recv1), "(1X,I0))"
  write(*, label // formstr) 4, mycomm%rank, &
      & "Recv1 buffer:", recv1

  ! I1 -> I2
  allocate(recv2(size(send1), mycomm%size))
  recv2(:,:) = 0
  send1(:) = [ mycomm%rank, mycomm%rank + 1 ]
  write(formstr, "(A,I0,A)") "A,", size(send1), "(1X,I0))"
  write(*, label // formstr) 5, mycomm%rank, &
      & "Send1 buffer:", send1(:)
  call mpifx_allgather(mycomm, send1, recv2)
  write(formstr, "(A,I0,A)") "A,", size(recv2), "(1X,I0))"
  write(*, label // formstr) 6, mycomm%rank, &
      & "Recv2 buffer:", recv2
  
  call mpifx_finalize()
  
end program test_allgather
