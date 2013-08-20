include(mpifx_finalize.m4)
  
module mpifx_finalize_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_finalize

contains

  subroutine mpifx_finalize(error)
    integer, intent(out), optional :: error

    integer :: error0

    call mpi_finalize(error0)
    call handle_errorflag(error0, "Error: mpi_finalize() in mpifx_finalize()", &
        & error)

  end subroutine mpifx_finalize
      
end module mpifx_finalize_module
