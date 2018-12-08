set(SOURCES-FPP
  libmpifx.F90
  mpifx_abort.F90
  mpifx_allgather.F90
  mpifx_allgatherv.F90
  mpifx_allreduce.F90
  mpifx_barrier.F90
  mpifx_bcast.F90
  mpifx_comm.F90
  mpifx_common.F90
  mpifx_constants.F90
  mpifx_finalize.F90
  mpifx_gather.F90
  mpifx_gatherv.F90
  mpifx_get_processor_name.F90
  mpifx_helper.F90
  mpifx_init.F90
  mpifx_recv.F90
  mpifx_reduce.F90
  mpifx_scatter.F90
  mpifx_send.F90)

set(SOURCES-F90-PREPROC)
foreach(fppsrc ${SOURCES-FPP})
  string(REGEX REPLACE "\\.F90" ".f90" f90src ${fppsrc})
  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${f90src}
    COMMAND m4 -I${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/${fppsrc} > ${CMAKE_CURRENT_BINARY_DIR}/${f90src}
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${fppsrc})
  list(APPEND SOURCES-F90-PREPROC ${CMAKE_CURRENT_BINARY_DIR}/${f90src})
endforeach()

add_library(mpifx ${SOURCES-F90-PREPROC})

set(INCLUDEDIR ${CMAKE_CURRENT_BINARY_DIR}/include)

set_target_properties(mpifx PROPERTIES Fortran_MODULE_DIRECTORY ${INCLUDEDIR})

target_include_directories(mpifx INTERFACE ${INCLUDEDIR})

install(TARGETS mpifx
  ARCHIVE DESTINATION ${INSTALL_LIB_DIR}
  LIBRARY DESTINATION ${INSTALL_LIB_DIR})

install(DIRECTORY ${INCLUDEDIR}/ DESTINATION ${INSTALL_MOD_DIR})
