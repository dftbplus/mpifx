set(tested
  test_allgather
  test_allgatherv)

set(targets
  ${tested}
  test_allreduce
  test_bcast
  test_comm_split
  test_comm_split_type
  test_gather
  test_gatherv
  test_reduce
  test_scatter
  test_scatterv
  test_win_shared_mem)

set(sources-helper
  testhelper.f90)

add_library(mpifxtesthelp ${sources-helper})
target_link_libraries(mpifxtesthelp PRIVATE MPI::MPI_Fortran MpiFx)

foreach(target IN LISTS targets)
  add_executable(${target} ${target}.f90)
  target_link_libraries(${target} MpiFx mpifxtesthelp)
endforeach()

foreach(target IN LISTS tested)
  add_test(NAME ${target}
    COMMAND ${MPIEXEC_EXECUTABLE}
    ${MPIEXEC_NUMPROC_FLAG}
    ${MPIEXEC_MAX_NUMPROCS}
    ${MPIEXEC_PREFLAGS}
    ${CMAKE_CURRENT_BINARY_DIR}/${target}
    ${MPIEXEC_POSTFLAGS})
  set_tests_properties(${target} PROPERTIES
    # test cases generate this on stdOut
    PASS_REGULAR_EXPRESSION "TestPASSED")
  set_tests_properties(${target} PROPERTIES
    # test cases generate this on stdOut
    FAIL_REGULAR_EXPRESSION "TestFAILED")
endforeach()
