set(sources-fpp
  module.fpp
  mpifx_abort.fpp
  mpifx_allgather.fpp
  mpifx_allgatherv.fpp
  mpifx_allreduce.fpp
  mpifx_barrier.fpp
  mpifx_bcast.fpp
  mpifx_comm.fpp
  mpifx_common.fpp
  mpifx_constants.fpp
  mpifx_finalize.fpp
  mpifx_gather.fpp
  mpifx_gatherv.fpp
  mpifx_get_processor_name.fpp
  mpifx_helper.fpp
  mpifx_init.fpp
  mpifx_recv.fpp
  mpifx_reduce.fpp
  mpifx_scatter.fpp
  mpifx_scatterv.fpp
  mpifx_send.fpp)

fypp_preprocess("${sources-fpp}" sources-f90)

# NAG compiler won't compile these files without the '-mismatch' option
if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "NAG")
  set_source_files_properties(SOURCE ${sources-f90} PROPERTY COMPILE_FLAGS -mismatch)
endif()

add_library(mpifx ${sources-f90})

target_link_libraries(mpifx PRIVATE MPI::MPI_Fortran)

set(BUILD_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR}/include)

set_target_properties(mpifx PROPERTIES Fortran_MODULE_DIRECTORY ${BUILD_MOD_DIR})

target_include_directories(mpifx PUBLIC
  $<BUILD_INTERFACE:${BUILD_MOD_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${INSTALL_MODULEDIR}>)

install(TARGETS mpifx
  EXPORT mpifx-targets
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})

if(INSTALL_INCLUDE_FILES)
  install(DIRECTORY ${BUILD_MOD_DIR}/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${INSTALL_MODULEDIR})
endif()
