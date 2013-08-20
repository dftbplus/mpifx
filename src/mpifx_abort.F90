include(mpifx_abort.m4)
  
module mpifx_abort_module
  use mpifx_common_module
  use mpifx_comm_module
  implicit none
  private

  public :: mpifx_abort

contains

  !> Aborts MPI processes for the given communicator.
  !! \param mympi  MPI handler.
  !! \param errorcode  Exit error code for the operating system. (default: -1)
  !! \param error  Optional error flag.
  !!
  subroutine mpifx_abort(mympi, errorcode, error)
    type(mpifx_comm), intent(in) :: mympi
    integer, intent(in), optional :: errorcode
    integer, intent(out), optional :: error
    
    integer :: error0, errorcode0

    _handle_inoptflag(errorcode0, errorcode, -1)
    call mpi_abort(mympi%id, errorcode0, error0)
    call handle_errorflag(error0, "MPI_ABORT in mpifx_abort", error)
    
  end subroutine mpifx_abort


end module mpifx_abort_module
