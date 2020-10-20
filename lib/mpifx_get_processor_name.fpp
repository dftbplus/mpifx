!> Contains the extended MPI communicator.
module mpifx_get_processor_name_module
  use mpifx_helper_module
  use mpi
  implicit none
  private

  public :: mpifx_get_processor_name

contains

  !> Returns the name of the processor/machine on which current process runs.
  !!
  !! \param rankname  Name of the processor (machine) on return.
  !! \param error  Error flag on return.
  !!
  subroutine mpifx_get_processor_name(rankname, error)
    character(:), allocatable, intent(out) :: rankname
    integer, intent(out), optional :: error

    integer :: error0, length
    character(MPI_MAX_PROCESSOR_NAME) :: buffer

    call mpi_get_processor_name(buffer, length, error0)
    call handle_errorflag(error0, "mpi_get_processor_name() in mpifx_get_processor_name", error)
    if (error0 /= 0) then
      return
    end if
    rankname = buffer(1:length)

  end subroutine mpifx_get_processor_name


end module mpifx_get_processor_name_module
