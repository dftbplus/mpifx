program test_comm_split
  use libmpifx_module
  implicit none

  type(mpifx_comm) :: allproc, groupproc
  integer :: groupsize, mygroup

  call mpifx_init()
  call allproc%init()
  groupsize = allproc%size / 2
  mygroup = allproc%rank / groupsize
  call allproc%split(mygroup, allproc%rank, groupproc)
  write(*, "(3(A,1X,I0,1X))") "GLOBAL ID:", allproc%rank, "SUBGROUP", &
      & mygroup, "SUBGROUP ID", groupproc%rank
  call mpifx_finalize()
  
end program test_comm_split
