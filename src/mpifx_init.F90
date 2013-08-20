include(mpifx_init.m4)
  
module mpifx_init_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_init

contains

  subroutine mpifx_init(error)
    integer, intent(out), optional :: error

    integer :: error0

    call mpi_init(error0)
    call handle_errorflag(error0, "Error: mpi_init() in mpifx_init()", error)

  end subroutine mpifx_init
      
end module mpifx_init_module
