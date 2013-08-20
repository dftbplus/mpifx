program test_bcast
  use libmpifx_module

  integer, parameter :: dp = kind(1.0d0)
  integer, parameter :: sp = kind(1.0)

  type(mpifx_comm) :: mympi
  integer :: buffer(3)
  logical :: lbuffer(3)
  real(dp) :: rbuffer(2, 2)
  complex(sp) :: cbuffer
  character(5) :: text

  ! Integer vector
  call mpifx_init()
  call mympi%init()
  buffer(:) = 0
  print "(A,I2.2,A,3I5)", "CHK01:", mympi%iproc, ":", buffer
  if (mympi%master) then
    buffer(:) = [ 1, 2, 3 ]
  end if
  print "(A,I2.2,A,3I5)", "CHK02:", mympi%iproc, ":", buffer
  call mpifx_bcast(mympi, buffer)
  print "(A,I2.2,A,3I5)", "CHK03:", mympi%iproc, ":", buffer
  call mpifx_barrier(mympi)

  ! Logical vector
  lbuffer(:) = .false.
  print "(A,I2.2,A,3L5)", "CHK04:", mympi%iproc, ":", lbuffer
  if (mympi%master) then
    lbuffer(:) = [ .true., .false., .true. ]
  end if
  print "(A,I2.2,A,3L5)", "CHK05:", mympi%iproc, ":", lbuffer
  call mpifx_bcast(mympi, lbuffer)
  print "(A,I2.2,A,3L5)", "CHK06:", mympi%iproc, ":", lbuffer
  call mpifx_barrier(mympi)

  ! Real rank 2 array
  rbuffer(:,:) = 0.0_dp
  print "(A,I2.2,A,4F10.6)", "CHK07:", mympi%iproc, ":", rbuffer
  if (mympi%master) then
    rbuffer(:,:) = reshape([ real(dp) :: 1, 2, 3, 4 ], [ 2, 2 ])
  end if
  print "(A,I2.2,A,4F10.6)", "CHK08:", mympi%iproc, ":", rbuffer
  call mpifx_bcast(mympi, rbuffer)
  print "(A,I2.2,A,4F10.6)", "CHK09:", mympi%iproc, ":", rbuffer
  call mpifx_barrier(mympi)

  ! Complex scalar
  cbuffer = cmplx(0, 0, sp)
  print "(A,I2.2,A,2F10.6)", "CHK10:", mympi%iproc, ":", cbuffer
  if (mympi%master) then
    cbuffer = cmplx(-1, 1, sp)
  end if
  print "(A,I2.2,A,2F10.6)", "CHK11:", mympi%iproc, ":", cbuffer
  call mpifx_bcast(mympi, cbuffer)
  print "(A,I2.2,A,2F10.6)", "CHK12:", mympi%iproc, ":", cbuffer

  ! Character
  text = "     "
  print "(A,I2.2,A,A6)", "CHK13:", mympi%iproc, ":", text
  if (mympi%master) then
    text = "hello"
  end if
  print "(A,I2.2,A,A6)", "CHK14:", mympi%iproc, ":", text
  call mpifx_bcast(mympi, text)
  print "(A,I2.2,A,A6)", "CHK15:", mympi%iproc, ":", text

  call mpifx_finalize()
  
end program test_bcast
