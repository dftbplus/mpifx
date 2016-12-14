!> \mainpage Modern Fortran wrappers around MPI routines
!!
!! The open source library [MPIFX](https://www.bitbucket.org/dftbplus/mpifx) is
!! an effort to provide modern Fortran (Fortran 2003) wrappers around
!! routines of the MPI library to make their use as simple as possible.
!! 
!! For more information see the following sources:
!! * [Online documentation](https://dftbplus.bitbucket.org/mpifx/)
!!   for installation and usage of the library
!! * [API documentation](annotated.html) for the reference manual.
!! * [Project home page](https://www.bitbucket.org/dftbplus/mpifx/)
!!   for the source code, bug tracker and further information on the project.
!!
module libmpifx_module
  use mpifx_constants_module
  use mpifx_comm_module
  use mpifx_abort_module
  use mpifx_get_processor_name_module
  use mpifx_barrier_module
  use mpifx_bcast_module
  use mpifx_finalize_module
  use mpifx_init_module
  use mpifx_send_module
  use mpifx_recv_module
  use mpifx_reduce_module
  use mpifx_allreduce_module
  use mpifx_gather_module
  use mpifx_allgather_module
  use mpifx_scatter_module
  implicit none
  public
  
end module libmpifx_module
