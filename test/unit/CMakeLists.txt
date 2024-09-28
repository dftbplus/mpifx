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
  test_bcast.fpp
)
fypp_preprocess("${fypp_flags}" "${sources-fypp}" sources-fypp-f90)

set(testapp "testapp")
add_executable(${testapp})
target_sources(${testapp} PRIVATE ${sources-f90} ${sources-fypp-f90})
target_link_libraries(${testapp} mpifx Fortuno::fortuno_mpi)
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
