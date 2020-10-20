set(targets
  test_allgather
  test_allgatherv
  test_allreduce
  test_bcast
  test_comm_split
  test_gather
  test_gatherv
  test_reduce
  test_scatter
  test_scatterv)

foreach(target IN LISTS targets)
  add_executable(${target} ${target}.f90)
  target_link_libraries(${target} MpiFx)
endforeach()
