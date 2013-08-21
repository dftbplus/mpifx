program test_send_recv
  use libmpifx_module
  implicit none

  character(100) :: msg
  type(mpifx_comm) :: mycomm
  integer :: source

  call mpifx_init()
  call mycomm%init()
  if (.not. mycomm%master) then
    write(msg, "(A,I0,A)") "Hello from process ", mycomm%iproc, "!"
    call mpifx_send(mycomm, msg, mycomm%imaster)
  else
    write(*, "(A)") "Master node:"
    do source = 1, mycomm%nproc - 1
      call mpifx_recv(mycomm, msg, source)
      write(*,"(A,A)") "Message received: ", trim(msg)
    end do
  end if
  call mpifx_finalize()
  
end program test_send_recv
