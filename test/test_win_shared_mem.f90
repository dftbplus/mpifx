program test_win_shared_mem
  use libmpifx_module
  implicit none

  type(mpifx_comm) :: globalcomm, nodecomm
  type(mpifx_win) :: win
  integer, parameter :: length = 7
  integer, pointer :: data_pointer(:)

  call mpifx_init()
  call globalcomm%init()

  ! Create a new communicator for all ranks on a node first
  call globalcomm%split_type(MPI_COMM_TYPE_SHARED, globalcomm%rank, nodecomm)

  call win%allocate_shared(nodecomm, length, data_pointer)

  call win%lock()

  ! Only rank 0 writes data into the array
  if (nodecomm%lead) then
    data_pointer(:) = 42
  end if

  call win%sync()
  call win%unlock()

  ! All ranks on the node will read the same value
  write(*, "(2(A,1X,I0,1X))") "ID:", nodecomm%rank, "VALUE:", data_pointer(1)

  call win%free()
  call mpifx_finalize()

end program test_win_shared_mem
