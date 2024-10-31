!> Helper routines for testers
module testhelper
  use libmpifx_module, only : mpifx_comm, mpifx_barrier, mpifx_finalize
  implicit none

  private
  public :: testReturn

contains

  !> Return expected labels for ctest
  subroutine testReturn(mycomm, tPassed)

    type(mpifx_comm), intent(in) :: mycomm

    logical, intent(in) :: tPassed

    call mpifx_barrier(mycomm)

    if (tPassed) then
      if (mycomm%rank == 0) then
        ! label for ctest regex
        write(*,*)'TestPASSED'
      end if
    else
      if (mycomm%rank == 0) then
        ! label for ctest regex
        write(*,*)'TestFAILED'
      end if
      call mpifx_finalize()
      stop
    end if

  end subroutine testReturn

end module testhelper
