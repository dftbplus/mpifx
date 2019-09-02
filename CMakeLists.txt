cmake_minimum_required(VERSION 3.5)

project(mpifx VERSION 0.1 LANGUAGES Fortran)

set(LIBRARY_ONLY FALSE CACHE BOOL "Whether only library should be compiled")

# Installation paths
set(INSTALL_BIN_DIR "${CMAKE_INSTALL_PREFIX}/bin" CACHE PATH
  "Installation directory for executables")

set(INSTALL_LIB_DIR "${CMAKE_INSTALL_PREFIX}/lib" CACHE PATH
  "Installation directory for libraries")

set(INSTALL_MOD_DIR "${CMAKE_INSTALL_PREFIX}/include/dftd3" CACHE PATH
  "Installation directory for Fortran module files")

set(INSTALL_CMAKE_DIR "${CMAKE_INSTALL_PREFIX}/lib/cmake" CACHE PATH
  "Installation directory for CMake package export files")

option(BUILD_SHARED_LIBS "Whether the library should be shared" FALSE)

option(INSTALL_INCLUDE_FILES "Whether include and module files should be installed" TRUE)

find_package(MPI REQUIRED)
find_program(FYPP fypp)
if(FYPP)
  message(STATUS "Preprocessor fypp: ${FYPP}")
else()
  message(FATAL_ERROR "Prepropcessor fypp not found")
endif()

add_subdirectory(lib)
if(NOT LIBRARY_ONLY)
  add_subdirectory(test)
endif()
