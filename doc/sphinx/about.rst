About MPIFX
===========

`MPIFX <https://github.com/dftbplus/mpifx/>`_ is a library containing modern
Fortran (Fortran 2003) wrappers around MPI routines. The goal is to make the use
of MPI as simple as possible in Fortran.

Consider for example a simple MPI broadcast. In order to broadcast an integer
array with 25 elements using the legacy MPI routine, you have to issue::

    call mpi_bcast(myarray, 25, MPI_INTEGER, 0, MPI_COMM_WORLD, error)

Additional to the object to be broadcasted and the communicator, you also
*must* specify following arguments:

- type of the array (which is redundant, as it is *known* at compile-time)

- size of the array (which is redundant, as it is *known* at run-time)

- root node of the broadcast (setting it to the lead node as default would
  be a definitely safe choice)

- error flag (one could per default just omit it and rely on the program to stop
  if a problem arised, similar as done in Fortran for allocations)

Using MPIFX the call above is as simple as::

    call mpifx_bcast(comm, myarray)

No redundant arguments, sensible defaults. Nevertheless the full functionality
is still available via optional parameters if needed. E.g. if you wanted to
handle the error flag yourself (making sure an error won't stop your code), you
could call::

    call mpifx_bcast(comm, myarray, error=ierr)

A few essential communication routines are already covered (see
:ref:`sec_routines`). If your desired MPI-routine is not among them yet, you are
cordially invited to extend MPIFX and to share it in order to let others profit
from your work (MPIFX is licensed under the simplified BSD license). For more
details see the `project page <https://github.com/dftbplus/mpifx/>`_.
