cmake_minimum_required(VERSION 3.16)

include(CMakePackageConfigHelpers)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
include(MpiFxUtils)

include(${CMAKE_CURRENT_SOURCE_DIR}/config.cmake)

project(MpiFx LANGUAGES Fortran)

# Get version number from file
file (STRINGS "${PROJECT_SOURCE_DIR}/VERSION" PROJECT_VERSION)
string(REPLACE "." ";" VERSION_LIST ${PROJECT_VERSION})
list(GET VERSION_LIST 0 PROJECT_VERSION_MAJOR)
list(GET VERSION_LIST 1 PROJECT_VERSION_MINOR)
list(GET VERSION_LIST 2 PROJECT_VERSION_PATCH)

setup_build_type()

#
# Prerequisites
#
find_package(MPI REQUIRED)
find_program(FYPP fypp)
if(NOT FYPP)
  message(FATAL_ERROR "Preprocessor fypp could not be found")
endif()

#
# Build instructions
#
include(GNUInstallDirs)

add_subdirectory(lib)
include(CTest) # note: this adds a BUILD_TESTING which defaults to ON
if(BUILD_TESTING)
  enable_testing()
  add_subdirectory(test)
endif()

#
# Installation
#
add_library(MpiFx INTERFACE)
target_link_libraries(MpiFx INTERFACE mpifx)
install(TARGETS MpiFx EXPORT mpifx-targets)

install(EXPORT mpifx-targets
  FILE mpifx-targets.cmake
  NAMESPACE MpiFx::
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/mpifx")

configure_package_config_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/utils/export/mpifx-config.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/cmake/mpifx-config.cmake
  INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/mpifx)

write_basic_package_version_file(
  ${CMAKE_CURRENT_BINARY_DIR}/cmake/mpifx-config-version.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY SameMajorVersion)

install(
  FILES ${CMAKE_CURRENT_BINARY_DIR}/cmake/mpifx-config.cmake
  ${CMAKE_CURRENT_BINARY_DIR}/cmake/mpifx-config-version.cmake
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/mpifx)

get_pkgconfig_params(PKGCONFIG_REQUIRES PKGCONFIG_LIBS PKGCONFIG_LIBS_PRIVATE PKGCONFIG_C_FLAGS)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/utils/export/mpifx.pc.in
  ${CMAKE_CURRENT_BINARY_DIR}/mpifx.pc @ONLY)
install(
  FILES "${CMAKE_CURRENT_BINARY_DIR}/mpifx.pc"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/pkgconfig")
