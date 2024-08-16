# Variables influencing how subproject is obtained
set(CMAKE_REQUIRE_FIND_PACKAGE_FortunoMpi ${MYPROJECT_SUBPROJECT_REQUIRE_FIND})
set(CMAKE_DISABLE_FIND_PACKAGE_FortunoMpi ${MYPROJECT_SUBPROJECT_DISABLE_FIND})
# set FETCHCONTENT_SOURCE_DIR_FORTUNO to use a local source of the subproject

# Subproject related variables
option(
  FORTUNO_MPI_BUILD_SHARED_LIBS "Fortuno: Build as shared library" ${MYPROJECT_BUILD_SHARED_LIBS}
)

# Make subproject available
FetchContent_Declare(
  FortunoMpi
  GIT_REPOSITORY "https://github.com/aradi/fortuno-mpi.git"
  GIT_TAG "fypp"
  FIND_PACKAGE_ARGS
)
FetchContent_MakeAvailable(FortunoMpi)

if (FortunoMpi_FOUND)
  message(STATUS "Subproject FortunoMpi: using installed version")
else ()
  message(STATUS "Subproject FortunoMpi: building from source in ${fortunompi_SOURCE_DIR}")
endif ()
