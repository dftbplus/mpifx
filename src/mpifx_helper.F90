include(mpifx_helper.m4)

!> Exports constants and helper routine(s).
!! \cond HIDDEN
module mpifx_helper_module
  use mpi
  implicit none
  private

  public :: default_tag, sp, dp
  public :: handle_errorflag

  !> Default tag
  integer, parameter :: default_tag = 0

  !> Single precision kind.
  integer, parameter :: sp = kind(1.0)

  !> Double precision kind.
  integer, parameter :: dp = kind(1.0d0)

contains

  !> Handles optional error flag.
  !!
  !! \param error0  Error flag as returned by some routine.
  !! \param msg  Msg to print out, if program is stopped.
  !! \param error  Optional error flag. If present, error0 is passed to it,
  !!     otherwise if error0 was not zero, the error message in msg is printed
  !!     and the program is stopped.
  !!
  subroutine handle_errorflag(error0, msg, error)
    integer, intent(in) :: error0
    character(*), intent(in) :: msg
    integer, intent(out), optional :: error

    integer :: aborterror
  
    if (present(error)) then
      error = error0
    elseif (error0 /= 0) then
      write(*, "(A)") "Operation failed!"
      write(*, "(A)") msg
      write(*, "(A,I0)") "Error: ", error0
      call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
      if (aborterror /= 0) then
        write(*, "(A)") "Stopping code did not succeed, hope for the best."
      end if
    end if

  end subroutine handle_errorflag


end module mpifx_helper_module

!> \endcond
