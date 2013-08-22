!> \mainpage Fortran 2003 wrappers around MPI routines
!! 
module libmpifx_module
  use mpifx_constants_module
  use mpifx_comm_module
  use mpifx_abort_module
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
