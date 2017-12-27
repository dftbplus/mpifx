Compiling and installing MPIFX
==============================

In order to compile MPIFX, you need following prerequisites:

* Fortran 2003 compiler,

* Python (2.6, 2.7 or any 3.x release)

* GNU Make.

There are basically two different ways of using the library in your project:

* `Precompiling the library`_ and linking it later to your project.

* `Compiling the library during your build process`_.

Both are described below.


Precompiling the library
************************

In order to create a precompiled library

#. Copy the file `make.arch.template` to `make.arch` in the root directory of
   the source and customize the settings for the compilers and the linker
   according to your system.

#. Issue `make` to build the library.

#. Issue `make install` to copy the library and the module files to the
   installation destination.

During the build process of your project, you may link the library with the
`-lmpifx` option.  Eventually, you may need to specify options for your compiler
and your linker to specify the location of those directories. Assuming you've
put the module files in the directory `<MODFILEDIR>` and the library file in
`<LIBRARYDIR>`, you would typically invoke your compiler for the source files
using the `libmpifx_module` as::

    F2003_COMPILER -I<MODFILEDIR> -c somesource.f90

and link your object files at the end with::

    LINKER -I<LIBRARYDIR> somesource.o ... -L<LIBRARYDIR> -lmpifx


Compiling the library during your build process
***********************************************

In order to build the library during the build process of your project:

#. Copy the content of the `lib/` folder into a *separate* folder within your
   project.

#. During the make process of your project, invoke the library makefile
   (`make.build`) to build the module files and the library in the folder
   where you've put the library sources.

   You must pass the compiler and linker options via variable defintions at the
   make command line. Assuming that the variables `$(FXX)`, `$(FXXOPT)`, `$(LN)`
   and `$(LNOPT)`, `$(FYPP)` and `$(FYPPOPT)` contain the Fortran compiler, the
   Fortran compiler options, the linker, the linker options, the Fypp
   preprocessor and its options, respectively, you would have something like::

       libmpifx.a:
               $(MAKE) -C $(MPIFX_BUILDDIR) \
                   FXX="$(FXX)" FXXOPT="$(FXXOPT)" \
                   LN="$(LN)" LNOPT="$(LNOPT)" \
                   FYPP="$(FYPP)" FYPPOPT="$(FYPPOPT)" \
                   -f $(MPIFX_SRCDIR)/make.build

   in the makefile of your project with `$(MPIFX_SRCDIR)` being the directory
   where you've put the source of MPIFX and `$(MPIFX_BUILDDIR)` where the build
   of the library should be done.

You should also have a look at the `Umakefile` in the root folder of MPIFX,
which uses exactly the same technique to compile the library.
