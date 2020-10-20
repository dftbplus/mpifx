#:include 'mpifx.fypp'

!> Contains wrapper for \c MPI_BARRIER.  
module mpifx_barrier_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_barrier

contains

  !> Sets a barrier.
  !!
  !! \param mycomm  MPI communicator.
  !! \param error  Optional error flag.
  !!
  !! Example:
  !!
  !!     program test_barrier
  !!       use libmpifx_module
  !!       implicit none
  !!
  !!       type(mpifx_comm) :: mycomm
  !!
  !!       call mpifx_init()
  !!       call mycomm%init()
  !!       :
  !!       ! Processes will wait until all processes arrive here.
  !!       call mpifx_barrier(mycomm)
  !!       :
  !!
  !!     end program test_barrier
  !!
  subroutine mpifx_barrier(mycomm, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(out), optional :: error
    
    integer :: error0
    
    call mpi_barrier(mycomm%id, error0)
    call handle_errorflag(error0, "MPI_BARRIER in mpifx_barrier", error)
    
  end subroutine mpifx_barrier


end module mpifx_barrier_module
