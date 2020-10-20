!> Contains wrapper for \c MPI_FINALIZE.  
module mpifx_finalize_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_finalize

contains

  !> Finalizes the MPI framework.
  !!
  !! \param error Error code on return. If not present and error code would have
  !!     been non-zero, routine aborts program execution.
  !!
  !! \see MPI documentation (\c MPI_FINALIZE)
  !!
  !! Example:
  !!
  !!     program test_mpifx
  !!       use libmpifx_module
  !!       implicit none
  !!
  !!       type(mpifx_comm) :: mycomm
  !!
  !!       call mpifx_init()
  !!       call mycomm%init()
  !!       :
  !!       call mpifx_finalize()
  !!
  !!     end program test_mpifx
  !!
  subroutine mpifx_finalize(error)
    integer, intent(out), optional :: error

    integer :: error0

    call mpi_finalize(error0)
    call handle_errorflag(error0, "Error: mpi_finalize() in mpifx_finalize()", error)

  end subroutine mpifx_finalize
      
end module mpifx_finalize_module
