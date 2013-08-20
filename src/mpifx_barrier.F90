include(mpifx_barrier.m4)
  
module mpifx_barrier_module
  use mpifx_common_module
  use mpifx_comm_module
  implicit none
  private

  public :: mpifx_barrier

contains

  !> Sets a barrier.
  !! \param mympi  MPI handler.
  !! \param error  Optional error flag.
  subroutine mpifx_barrier(mympi, error)
    type(mpifx_comm), intent(in) :: mympi
    integer, intent(out), optional :: error
    
    integer :: error0
    
    call mpi_barrier(mympi%id, error0)
    call handle_errorflag(error0, "MPI_BARRIER in mpifx_barrier", error)
    
  end subroutine mpifx_barrier


end module mpifx_barrier_module
