program test_shared_memory
  use libmpifx_module
  implicit none

  type(mpifx_comm) :: globalcomm, nodecomm
  integer, parameter :: length = 7
  integer :: win
  integer, pointer :: data_pointer(:)

  call mpifx_init()
  call globalcomm%init()

  ! Create a new communicator for all ranks on a node first
  call globalcomm%split_type(MPI_COMM_TYPE_SHARED, globalcomm%rank, nodecomm)

  call mpifx_allocate_shared(nodecomm, length, win, data_pointer)

  call mpifx_lock_shared(win)

  ! Only rank 0 writes data into the array
  if (nodecomm%lead) then
    data_pointer(:) = 42
  end if

  call mpifx_sync_shared(nodecomm, win)
  call mpifx_unlock_shared(win)

  ! All ranks on the node will read the same value
  write(*, "(2(A,1X,I0,1X))") "ID:", nodecomm%rank, "VALUE:", data_pointer(1)

  call mpifx_free_shared(win)
  call mpifx_finalize()

end program test_shared_memory
