!> \mainpage Fortran 2003 wrappers around MPI routines
!! 
module libmpifx_module
  use mpifx_comm_module
  use mpifx_bcast_module
  use mpifx_send_recv_module
  use mpifx_barrier_module
  use mpifx_abort_module
  use mpifx_init_module
  use mpifx_finalize_module
  implicit none
  
  public
  
end module libmpifx_module
