program test_send_recv
  use libmpifx_module
  implicit none

  character(100) :: msg
  type(mpifx_comm) :: mympi
  integer :: source

  call mpifx_init()
  call mympi%init()
  if (.not. mympi%master) then
    write(msg, "(A,I0,A)") "Hello from process ", mympi%iproc, "!"
    call mpifx_send(mympi, msg, mympi%imaster)
  else
    write(*, "(A)") "Master node:"
    do source = 1, mympi%nproc - 1
      call mpifx_recv(mympi, msg, source)
      write(*,"(A,A)") "Message received: ", trim(msg)
    end do
  end if
  call mpifx_finalize()
  
end program test_send_recv
