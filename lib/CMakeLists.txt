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
  mpifx_send.fpp)

set(sources-f90-preproc)

foreach(fppsrc IN LISTS sources-fpp)
  string(REGEX REPLACE "\\.fpp" ".f90" f90src ${fppsrc})
  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${f90src}
    COMMAND ${FYPP} -I${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/${fppsrc} ${CMAKE_CURRENT_BINARY_DIR}/${f90src}
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${fppsrc})
  list(APPEND sources-f90-preproc ${CMAKE_CURRENT_BINARY_DIR}/${f90src})
endforeach()

# NAG compiler won't compile this files without the '-mismatch' option
if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "NAG")
  set_source_files_properties(SOURCE ${sources-f90-preproc} PROPERTY COMPILE_FLAGS -mismatch)
endif()

add_library(mpifx ${sources-f90-preproc})

set(BUILD_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR}/include)

set_target_properties(mpifx PROPERTIES Fortran_MODULE_DIRECTORY ${BUILD_MOD_DIR})

target_include_directories(mpifx PUBLIC
  $<BUILD_INTERFACE:${BUILD_MOD_DIR}>
  $<INSTALL_INTERFACE:${INSTALL_MOD_DIR}>)

install(TARGETS mpifx
  EXPORT ${INSTALL_EXPORT_NAME}
  ARCHIVE DESTINATION ${INSTALL_LIB_DIR}
  LIBRARY DESTINATION ${INSTALL_LIB_DIR})

if(INSTALL_INCLUDE_FILES)
  install(DIRECTORY ${BUILD_MOD_DIR}/ DESTINATION ${INSTALL_MOD_DIR})
endif()
