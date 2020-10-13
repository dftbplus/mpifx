#
# Build options
#

# CMAKE_BUILD_TYPE is commented out in order to allow for multi-configuration builds. It will
# automatically default to RelWithDebInfo if used in a single configuration build. Uncomment or
# override it only if you want a non-default single configuration build.
#
#set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "Build type (Release|RelWithDebInfo|Debug|MinSizeRel)")

# If set to True, only those public targets (typically the library) will be built, which are usually
# exported via CMake export files. Otherwise all targets all built (default case). Set this option
# to True, if you invoke this project as part of an other CMake project via the add_subdirectory()
# command without the EXCLUDE_FROM_ALL option (e.g. if you want this project to install its targets
# as part of the top projects installation process).
#
option(BUILD_EXPORTED_TARGETS_ONLY
  "Whether only exported targets (the library, but no tests) should be built" FALSE)

option(BUILD_SHARED_LIBS "Whether the library should be a shared one" FALSE)

#
# Installation options
#

option(INSTALL_INCLUDE_FILES "Whether include / module files should be installed" TRUE)

set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}/_install" CACHE STRING
  "Directory to install the compiled code into")

set(INSTALL_INCLUDEDIR "mpifx" CACHE PATH
  "Installation directory for header and include files (within standard include folder)")

set(INSTALL_MODULEDIR "${INSTALL_INCLUDEDIR}/modfiles" CACHE PATH
  "Installation directory for Fortran module files (within standard include folder)")
