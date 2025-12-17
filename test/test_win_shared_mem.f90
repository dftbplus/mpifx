program test_win_shared_mem
  use libmpifx_module
  implicit none

  ! global communicator and within each shared memory node
  type(mpifx_comm) :: globalcomm, nodecomm
  ! RMA window, in this case shared memory
  type(mpifx_win) :: win
  ! Value to store for testing
  integer, parameter :: sample_value = 42
  ! Specific local sub-region sizes for one of the tests, either on the leader or followers in a
  ! node
  integer, parameter :: size_rank_0 = 7, size_rank_other = 4
  ! Global and local sizes of array in window
  integer(MPIFX_SIZE_T) :: global_length, local_length

  integer :: global_length_int32, local_length_int32
  integer :: rank, ii
  ! Pointer to whole array in window and the local part
  integer, pointer :: global_pointer(:), local_pointer(:)

  call mpifx_init()
  call globalcomm%init()

  ! Create a new communicator for all ranks that are on the same node first
  call globalcomm%split_type(MPI_COMM_TYPE_SHARED, globalcomm%rank, nodecomm)

  if (nodecomm%lead) then
    local_length = size_rank_0
  else
    local_length = size_rank_other
  end if
  global_length = size_rank_0 + size_rank_other * (nodecomm%size - 1)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! First example, global array, distributed with only one process on the node writing
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Allocate a global window on the node
  call win%allocate_shared(nodecomm, global_length, global_pointer)

  call win%lock_all()

  ! Only rank 0 writes data into the array
  if (nodecomm%lead) then
    global_pointer(:) = sample_value
  end if

  call win%sync()
  call win%unlock_all()

  ! All ranks on the node will read the same value in the global array view
  if (any(global_pointer(1:global_length) /= sample_value)) then
    write(*, "(3(A,1X,I0,1X))") "ERROR! ID:", nodecomm%rank, "VALUE:", global_pointer(1),&
        & "EXPECTED:", sample_value
    call mpifx_abort(globalcomm)
  end if

  call win%free()

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! Second example, global array, lead rank writing to all of it, then local parts being written by
  !! individual ranks on the node
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Initialize again with specific local length
  call win%allocate_shared(nodecomm, global_length, global_pointer, local_length, local_pointer)

  call win%fence(MPI_MODE_NOSTORE + MPI_MODE_NOPRECEDE)

  ! Only rank 0 writes data into the array
  if (nodecomm%lead) then
    global_pointer(:) = sample_value
  end if

  call win%fence()

  ! All ranks on the node will read the same value in their local view
  if (any(local_pointer(1:local_length) /= sample_value)) then
    write(*, "(2(A,1X,I0,1X))") "ERROR! ID:", nodecomm%rank, "VALUE:", local_pointer(1),&
        & "EXPECTED:", sample_value
    call mpifx_abort(globalcomm)
  end if

  ! Now let all ranks write something into their local chunk
  local_pointer(1:local_length) = nodecomm%rank

  call win%fence()

  ! All ranks should now be able to read the correct global values
  if (any(global_pointer(1:size_rank_0) /= 0)) then
    write(*, "(2(A,1X,I0,1X))") "ERROR! ID:", nodecomm%rank, "VALUE:", global_pointer(1),&
        & "EXPECTED:", 0
    call mpifx_abort(globalcomm)
  end if
  do rank = 1, nodecomm%size - 1
    ii = size_rank_0 + 1 + size_rank_other * (rank - 1)
    if (any(global_pointer(ii:ii+size_rank_other-1) /= rank)) then
      write(*, "(2(A,1X,I0,1X))") "ERROR! ID:", nodecomm%rank, "VALUE:", global_pointer(ii),&
          & "EXPECTED:", rank
      call mpifx_abort(globalcomm)
    end if
  end do

  call win%free()

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! 32 bit sized indexing as a test
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Initialize again with int32 sizes
  global_length_int32 = global_length
  local_length_int32 = local_length
  call win%allocate_shared(nodecomm, global_length_int32, global_pointer, local_length_int32,&
      & local_pointer)

  call win%free()
  call mpifx_finalize()

end program test_win_shared_mem
