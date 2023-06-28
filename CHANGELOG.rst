**********
Change Log
**********

Notable project changes in various releases.

1.4
===

Added
-----

* mpifx_win can now allocate memory on each rank, if desired.


1.3.1
=====

Fixed
-----

* Compilation error in unit tests

* Standard CMake option ``BUILD_TESTING`` to disable building unit tests


1.3
===

Added
------

* Grid splitting based on type (e.g. MPI_COMM_TYPE_SHARED)

* Wrappers for accessing MPI shared memory window

* Some tests accessible via ctest


1.2
===

Added
-----

* Support for building with meson

* Support to build with GNU Fortran >= 11


1.1
===

Added
-----

* Method 'free' implemented to release the MPI-communicator


1.0
===

Added
-----

* Various improvements in the CMake-build system.

* CMake and PKG-Config export files when MpiFx is installed.


Changed
-------

* The Fypp-preprocessor is not shipped with MpiFx but is an external
  requirement.

* Name convention for processes (master -> lead).
