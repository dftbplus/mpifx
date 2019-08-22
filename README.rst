****************************************
MPIFX - Modern Fortran Interface for MPI
****************************************

The open source library `MPIFX <https://github.com/dftbplus/mpifx>`_ is
an effort to provide modern Fortran (Fortran 2003) wrappers around
routines of the MPI library to make their use as simple as possible. The
documentation is included inside the repository, but is also available at
`dftbplus.github.io <https://dftbplus.github.io/>`_.

It currently contains only a few routines so far, but if those happen to be the
ones you need, feel free to use this project. MPIFX is licensed under the
**simplified BSD license**.

If your desired MPI routine is not yet wrapped up, feel free to contribute to
the project to include the target functionality.


INSTALL
=======

Stand-alone building
--------------------

#. Make a copy of the file `make.arch.template` as `make.arch`::

       cp make.arch.template make.arch

#. Configure any settings in `make.arch` in order to adapt it to your
   environment.

#. Issue ::

       make

   in order to build and library and ::

       make install

   in order to install it.

#. You may build the examples in the `test/` subfolder with ::

       make test


 
Build the library as part of a build process
--------------------------------------------

You may build the library on-the-fly during the build of your program. Invoke
the library makefile `lib/make.build` during your build process from the folder
where you wish to build the library. Make sure to pass the necessary
make-variables (as documented in the library makfile). See the `makefile` in
this folder for an example how to invoke the library makefile.
