set(tested
  test_allgather
  test_allgatherv
  )
set(targets
  test_allreduce
  test_bcast
  test_comm_split
  test_gather
  test_gatherv
  test_reduce
  test_scatter
  test_scatterv
  )
list(APPEND targets ${tested})

foreach(target IN LISTS targets)
  add_executable(${target} ${target}.f90 testhelper.f90)
  target_link_libraries(${target} MpiFx)
endforeach()

foreach(target IN LISTS tested)
  add_test(NAME ${target}
    COMMAND ${MPIEXEC_EXECUTABLE}
    ${MPIEXEC_NUMPROC_FLAG}
    ${MPIEXEC_MAX_NUMPROCS}
    ${MPIEXEC_PREFLAGS}
    ${CMAKE_CURRENT_BINARY_DIR}/${target}
    ${MPIEXEC_POSTFLAGS}
    )
  set_tests_properties(${target} PROPERTIES
    PASS_REGULAR_EXPRESSION "TestPASSED"
    )
  set_tests_properties(${target} PROPERTIES
    FAIL_REGULAR_EXPRESSION "TestFAILED"
    )
endforeach()
