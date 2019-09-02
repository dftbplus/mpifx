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

find_package(MPI REQUIRED)

add_subdirectory(src)
if(NOT LIBRARY_ONLY)
  add_subdirectory(test)
endif()
