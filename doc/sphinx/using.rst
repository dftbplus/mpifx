Using MPIFX
===========

Before you can use the MPIFX routines you need the following steps:

#. Use the module `libmpifx_module` in your routines.

#. Initialize the MPI framework via the `mpifx_init()` routine. (If you already
   initialized it via the legacy `mpi_init()` call, you should omit this step.

#. Initialize a communicator of `type(mpifx_comm)`.

Below you find a self containing example for reduction on all processes using
a wrapper around `mpi_allreduce()`::

    program test_allreduce
      use libmpifx_module
      implicit none

      integer, parameter :: dp = kind(1.0d0)

      type(mpifx_comm) :: mycomm
      integer :: vali0, resvali0
      real(dp) :: valr(3), resvalr(3)

      call mpifx_init()
      call mycomm%init()

      ! Reduce scalar value
      vali0 = mycomm%rank * 2  ! Some arbitrary number
      write(*, "(I2.2,'-',I3.3,'|',1X,A,I0)") 1, mycomm%rank, &
          & "Value to be operated on:", vali0
      call mpifx_allreduce(mycomm, vali0, resvali0, MPI_SUM)
      write(*, "(I2.2,'-',I3.3,'|',1X,A,I0)") 2, mycomm%rank, &
          & "Obtained result (sum):", resvali0

      ! Reduce vector
      valr(:) = [ real(mycomm%rank + 1, dp) * 1.2, &
          & real(mycomm%rank + 1, dp) * 4.3, real(mycomm%rank + 1, dp) * 3.8 ]
      write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 3, mycomm%rank, &
          & "Value to be operated on:", valr(:)
      call mpifx_allreduce(mycomm, valr, resvalr, MPI_PROD)
      write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 4, mycomm%rank, &
          & "Obtained result (prod):", resvalr(:)
      call mpifx_finalize()
      
    end program test_allreduce


When running on 4 processors::

    mpirun -n 4 test_allreduce | sort

you should obtain the following output::

    01-000| Value to be operated on:0
    01-001| Value to be operated on:2
    01-002| Value to be operated on:4
    01-003| Value to be operated on:6
    02-000| Obtained result (sum):12
    02-001| Obtained result (sum):12
    02-002| Obtained result (sum):12
    02-003| Obtained result (sum):12
    03-000| Value to be operated on:    1.20    4.30    3.80
    03-001| Value to be operated on:    2.40    8.60    7.60
    03-002| Value to be operated on:    3.60   12.90   11.40
    03-003| Value to be operated on:    4.80   17.20   15.20
    04-000| Obtained result (prod):   49.77 8205.12 5004.33
    04-001| Obtained result (prod):   49.77 8205.12 5004.33
    04-002| Obtained result (prod):   49.77 8205.12 5004.33
    04-003| Obtained result (prod):   49.77 8205.12 5004.33

Have a look at the test folder in the source tree for further examples.
