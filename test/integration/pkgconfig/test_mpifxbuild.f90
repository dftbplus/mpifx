program test_bcast
  use libmpifx_module
  implicit none
  
  integer, parameter :: dp = kind(1.0d0)
  integer, parameter :: sp = kind(1.0)

  type(mpifx_comm) :: mycomm
  integer :: buffer(3)
  logical :: lbuffer(3)
  real(dp) :: rbuffer(2, 2)
  complex(sp) :: cbuffer
  character(5) :: text

  ! Integer vector
  call mpifx_init()
  call mycomm%init()
  buffer(:) = 0
  print "(A,I2.2,A,3I5)", "CHK01:", mycomm%rank, ":", buffer
  if (mycomm%lead) then
    buffer(:) = [ 1, 2, 3 ]
  end if
  print "(A,I2.2,A,3I5)", "CHK02:", mycomm%rank, ":", buffer
  call mpifx_bcast(mycomm, buffer)
  print "(A,I2.2,A,3I5)", "CHK03:", mycomm%rank, ":", buffer
  call mpifx_barrier(mycomm)

  ! Logical vector
  lbuffer(:) = .false.
  print "(A,I2.2,A,3L5)", "CHK04:", mycomm%rank, ":", lbuffer
  if (mycomm%lead) then
    lbuffer(:) = [ .true., .false., .true. ]
  end if
  print "(A,I2.2,A,3L5)", "CHK05:", mycomm%rank, ":", lbuffer
  call mpifx_bcast(mycomm, lbuffer)
  print "(A,I2.2,A,3L5)", "CHK06:", mycomm%rank, ":", lbuffer
  call mpifx_barrier(mycomm)

  ! Real rank 2 array
  rbuffer(:,:) = 0.0_dp
  print "(A,I2.2,A,4F10.6)", "CHK07:", mycomm%rank, ":", rbuffer
  if (mycomm%lead) then
    rbuffer(:,:) = reshape([ real(dp) :: 1, 2, 3, 4 ], [ 2, 2 ])
  end if
  print "(A,I2.2,A,4F10.6)", "CHK08:", mycomm%rank, ":", rbuffer
  call mpifx_bcast(mycomm, rbuffer)
  print "(A,I2.2,A,4F10.6)", "CHK09:", mycomm%rank, ":", rbuffer
  call mpifx_barrier(mycomm)

  ! Complex scalar
  cbuffer = cmplx(0, 0, sp)
  print "(A,I2.2,A,2F10.6)", "CHK10:", mycomm%rank, ":", cbuffer
  if (mycomm%lead) then
    cbuffer = cmplx(-1, 1, sp)
  end if
  print "(A,I2.2,A,2F10.6)", "CHK11:", mycomm%rank, ":", cbuffer
  call mpifx_bcast(mycomm, cbuffer)
  print "(A,I2.2,A,2F10.6)", "CHK12:", mycomm%rank, ":", cbuffer

  ! Character
  text = "     "
  print "(A,I2.2,A,A6)", "CHK13:", mycomm%rank, ":", text
  if (mycomm%lead) then
    text = "hello"
  end if
  print "(A,I2.2,A,A6)", "CHK14:", mycomm%rank, ":", text
  call mpifx_bcast(mycomm, text)
  print "(A,I2.2,A,A6)", "CHK15:", mycomm%rank, ":", text

  call mpifx_finalize()
  
end program test_bcast
