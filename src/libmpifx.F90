!> \mainpage Modern Fortran wrappers around MPI routines
!!
!! The open source library [MPIFX](https://www.bitbucket.org/aradi/mpifx) is
!! an effort to provide modern Fortran (Fortran 2003) wrappers around
!! routines of the MPI library to make their use as simple as possible.
!! 
!! A few essential communication routines are already covered. See the
!! * [API DOCUMENTATION](annotated.html)
!!
!! whether the routines you need are there. If not, you are cordially invited to
!! extend MPIFX and to share it in order to let others profit from your
!! work. MPIFX is licensed under the **simplified BSD license**.
!!
!! Information about installation and usage of the library you find in the
!! [Wiki](https://www.bitbucket.org/aradi/mpifx/wiki).
!! Project status, current source code, bugtracker etc. are to be found on the
!! [MPIFX project home page](https://www.bitbucket.org/aradi/mpifx).
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
