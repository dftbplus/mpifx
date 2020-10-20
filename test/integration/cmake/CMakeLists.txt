cmake_minimum_required(VERSION 3.16)

project(TestMpiFxBuild LANGUAGES Fortran)

find_package(MpiFx REQUIRED)

add_executable(test_mpifxbuild test_mpifxbuild.f90)
target_link_libraries(test_mpifxbuild MpiFx::MpiFx)
