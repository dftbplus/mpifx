include(mpifx_init.m4)

!> Contains wrapper for \c MPI_INIT.
module mpifx_init_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_init

contains

  !> Initializes the MPI environment.
  !!
  !! \param error Error code on return. If not present and error code would have
  !!     been non-zero, routine aborts program execution.
  !!
  !! \see MPI documentation (\c MPI_INIT)
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
  subroutine mpifx_init(error)
    integer, intent(out), optional :: error

    integer :: error0

    call mpi_init(error0)
    call handle_errorflag(error0, "Error: mpi_init() in mpifx_init()", error)

  end subroutine mpifx_init
      
end module mpifx_init_module
