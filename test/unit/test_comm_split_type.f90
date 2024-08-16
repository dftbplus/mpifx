program test_split_type
  use libmpifx_module
  implicit none

  type(mpifx_comm) :: allproc, splitproc

  call mpifx_init()
  call allproc%init()
  call allproc%split_type(MPI_COMM_TYPE_SHARED, allproc%rank, splitproc)
  write(*, "(2(A,1X,I0,1X))") "ID:", allproc%rank, "SPLIT ID", splitproc%rank
  call mpifx_finalize()

end program test_split_type
