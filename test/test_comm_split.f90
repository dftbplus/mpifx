program test_comm_split
  use libmpifx_module
  implicit none

  type(mpifx_comm) :: allproc, groupproc
  integer :: groupsize, mygroup

  call mpifx_init()
  call allproc%init()
  groupsize = allproc%nproc / 2
  mygroup = allproc%iproc / groupsize
  call allproc%split(mygroup, allproc%iproc, groupproc)
  write(*, "(3(A,1X,I0,1X))") "GLOBAL ID:", allproc%iproc, "SUBGROUP", &
      & mygroup, "SUBGROUP ID", groupproc%iproc
  call mpifx_finalize()
  
end program test_comm_split
