set(sources-fpp
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

set(sources-f90-preproc)

foreach(fppsrc IN LISTS sources-fpp)
  string(REGEX REPLACE "\\.F90" ".f90" f90src ${fppsrc})
  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${f90src}
    COMMAND m4 -I${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/${fppsrc} > ${CMAKE_CURRENT_BINARY_DIR}/${f90src}
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${fppsrc})
  list(APPEND sources-f90-preproc ${CMAKE_CURRENT_BINARY_DIR}/${f90src})
endforeach()

add_library(mpifx_objlib OBJECT ${sources-f90-preproc})
add_library(mpifx $<TARGET_OBJECTS:mpifx_objlib>)

set(includedir ${CMAKE_CURRENT_BINARY_DIR}/include)

set_target_properties(mpifx_objlib PROPERTIES Fortran_MODULE_DIRECTORY ${includedir})

target_include_directories(mpifx_objlib PUBLIC ${includedir})
target_include_directories(mpifx PUBLIC ${includedir})

install(TARGETS mpifx
  ARCHIVE DESTINATION ${INSTALL_LIB_DIR}
  LIBRARY DESTINATION ${INSTALL_LIB_DIR})

install(DIRECTORY ${includedir}/ DESTINATION ${INSTALL_MOD_DIR})
