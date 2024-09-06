set(fypp_flags ${FYPP_FLAGS})
get_target_property(
  _fortuno_mpi_incdir
  Fortuno::fortuno_include_dir
  INTERFACE_INCLUDE_DIRECTORIES
)
list(APPEND fypp_flags "-I${_fortuno_mpi_incdir}" "-I${CMAKE_SOURCE_DIR}/src/dftbp/include")

set(unit-test-prefix "unit")

set(
  sources-f90
  testapp.f90
)
set(
  sources-fypp
  test_allgather.fpp
  test_allgatherv.fpp
  test_allreduce.fpp
  test_bcast.fpp
  test_comm_split_type.fpp
  test_comm_split.fpp
  test_gather.fpp
  test_gatherv.fpp
  test_reduce.fpp
  test_scatter.fpp
  test_scatterv.fpp
  test_send_recv.fpp
)
fypp_preprocess("${fypp_flags}" "${sources-fypp}" sources-fypp-f90)

set(testapp "testapp")
add_executable(${testapp})
target_sources(${testapp} PRIVATE ${sources-f90} ${sources-fypp-f90})
target_link_libraries(${testapp} mpifx Fortuno::fortuno_mpi MPI::MPI_Fortran)
add_test(
  NAME ${unit-test-prefix}
  COMMAND
    ${MPIEXEC_EXECUTABLE}
    ${MPIEXEC_NUMPROC_FLAG}
    ${MPIEXEC_MAX_NUMPROCS}
    ${MPIEXEC_PREFLAGS}
    ${CMAKE_CURRENT_BINARY_DIR}/${testapp}
    ${MPIEXEC_POSTFLAGS}
)
