****************************************
MpiFx - Modern Fortran Interface for MPI
****************************************

The open source library `MpiFx <https://github.com/dftbplus/mpifx>`_ provides
modern Fortran (Fortran 2008) wrappers around routines of the MPI library to
make their use as simple as possible. Currently several data distribution
routines are covered.

The documentation is included inside the repository, but is also available at
`dftbplus.github.io <https://dftbplus.github.io/>`_.


Installation
============

The preferred way of obtaining MpiFx is to install it via the Conda package
management framework using `Miniconda
<https://docs.conda.io/en/latest/miniconda.html>`_ or `Anaconda
<https://www.anaconda.com/products/individual>`_. Make sure to add/enable the
``conda-forge`` channel in order to be able to access MpiFx, and ensure that the
``conda-forge`` channel is the first repository to be searched for packages.

We provide both, OpenMPI and MPICH based build variants, choose the one suiting
your needs. For example, issue ::

  conda install 'mpifx=*=mpi_mpich_*'

or ::

  conda install 'mpifx=*=mpi_openmpi_*'

to get the last stable release of MpiFx for the respective MPI framework.


Building from source
====================

Prerequisites
-------------

* CMake (version >= 3.16)

* Fortran 2008 compatible Fortran compiler

* MPI-library and wrappers for your compiler supporting the `mpi_f08` interface

* `Fypp preprocessor <https://github.com/aradi/fypp>`_


Building and installing the library
-----------------------------------

The library can be built and installed with the usual CMake-workflow::

  FC=gfortran cmake -B _build -DCMAKE_INSTALL_PREFIX=$HOME/opt/mpifx
  cmake --build _build
  cmake --install _build

You can influence the configuration via CMake-variables, which are listed in
`config.cmake <config.cmake>`_. You can either modify the values directly there
or pass them as command line options at the configuration phase, e.g.::

  FC=ifort cmake -B _build -DBUILD_TESTING=NO .


Testing
-------

A few tests / usage examples can be found in the `test/` subdirectory,
a sub-set of which can be checked with ctest. The compiled test
programs will be in the `test/` subfolder of your build directory.

To disable building tests, include the cmake option -DBUILD_TESTING=OFF


Using the library
=================

CMake build
-----------

* Make sure to add the root folder of the installed library to the
  ``CMAKE_PREFIX_PATH`` environment variable.

* Use ``find_package()`` in `CMakeLists.txt` to locate the library and link
  ``MpiFx::MpiFx`` to every target which relies directly on the library ::

    cmake_minimum_required(VERSION 3.16)

    project(TestMpiFx LANGUAGES Fortran)

    find_package(MpiFx REQUIRED)

    add_executable(test_mpifx test_mpifx.f90)
    target_link_libraries(test_mpifx MpiFx::MpiFx)


Pkg-config build
----------------

* Make sure to add the `lib/pkgconfig` folder of the installed library to the
  ``PKG_CONFIG_PATH`` environment variable.

* Query the include and library options needed for the build with the usual
  ``pkg-config`` commands::

    mpifort $(pkg-config --cflags mpifx) test_mpifx.f90 $(pkg-config --libs mpifx)

  Note, that neither ``-cflags`` or ``--libs`` return any options related to
  your MPI-framework nor is the MPI-framework specified as dependency in the
  pkg-config file. Use the MPI-wrapper of your compiler to compile and link your
  executable or pass the additional include and library options by hand.


License
=======

MpiFx is licensed under the `2-Clause BSD License <LICENSE>`_.
