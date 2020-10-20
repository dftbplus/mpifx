!> Contains wrapper for \c MPI_ABORT.
module mpifx_abort_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_abort

contains

  !> Aborts MPI processes for the given communicator.
  !!
  !! \param mycomm  MPI handler.
  !! \param errorcode  Exit error code for the operating system. (default: 1)
  !! \param error  Optional error flag.
  !!
  !! \see MPI documentation (\c MPI_ABORT)
  !!
  !! Example:
  !!
  !!     program test_abort
  !!       use libmpifx_module
  !!       implicit none
  !!
  !!       type(mpifx_comm) :: mycomm
  !!
  !!       call mpifx_init()
  !!       call mycomm%init()
  !!       :
  !!       ! Stoping the program (e.g. due to error we can not handle)
  !!       call mpifx_abort(mycomm, 2)
  !!
  !!     end program test_abort
  !!
  subroutine mpifx_abort(mycomm, errorcode, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in), optional :: errorcode
    integer, intent(out), optional :: error

    integer :: error0, errorcode0

    if (present(errorcode)) then
      errorcode0 = errorcode
    else
      errorcode0 = -1
    end if

    call mpi_abort(mycomm%id, errorcode0, error0)
    call handle_errorflag(error0, "MPI_ABORT in mpifx_abort", error)

  end subroutine mpifx_abort

end module mpifx_abort_module
