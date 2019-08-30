#:include 'mpifx.fypp'
#:set OPT_ARG_RANKS = (0, 1)

!> Exports constants and helper routine(s).
!! \cond HIDDEN
module mpifx_helper_module
  use mpi
  use, intrinsic :: iso_fortran_env, only : stderr => error_unit
  use mpifx_constants_module
  implicit none
  private

  public :: default_tag, sp, dp
  public :: handle_errorflag, assert_failed
  public :: getoptarg, setoptarg

  !> Default tag
  integer, parameter :: default_tag = 0

  !> Single precision kind.
  integer, parameter :: sp = kind(1.0)

  !> Double precision kind.
  integer, parameter :: dp = kind(1.0d0)


  interface getoptarg
#:for RANK in OPT_ARG_RANKS
  #:for TYPE in ALL_TYPES
    module procedure getoptarg_${TYPE_ABBREVS[TYPE]}$${RANK}$
  #:endfor
#:endfor
  end interface getoptarg


  interface setoptarg
#:for RANK in OPT_ARG_RANKS
  #:for TYPE in ALL_TYPES
    module procedure setoptarg_${TYPE_ABBREVS[TYPE]}$${RANK}$
  #:endfor
#:endfor
  end interface setoptarg


contains

  !> Handles optional error flag.
  !!
  subroutine handle_errorflag(error0, msg, error)

    !> Error flag as returned by some routine.
    integer, intent(in) :: error0

    !>  Msg to print out, if program is stopped.
    character(*), intent(in) :: msg

    !> Optional error flag.
    !!
    !! If present, error0 is passed to it, otherwise if error0 was not zero, the
    !! error message in msg is printed and the program is stopped.
    !!
    integer, intent(out), optional :: error

    integer :: aborterror

    if (present(error)) then
      error = error0
    elseif (error0 /= 0) then
      write(stderr, "(A)") "Operation failed!"
      write(stderr, "(A)") msg
      write(stderr, "(A,I0)") "Error: ", error0
      call mpi_abort(MPI_COMM_WORLD, MPIFX_UNHANDLED_ERROR, aborterror)
      if (aborterror /= 0) then
        write(stderr, "(A)") "Stopping code with 'mpi_abort' did not succeed, trying 'stop' instead"
        stop 1
      end if
    end if

  end subroutine handle_errorflag


  !> Stops code signalizing a failed assert condition
  !!
  subroutine assert_failed(file, line)
    character(*), intent(in) :: file
    integer, intent(in) :: line

    integer :: aborterror

    write(stderr, "(A)") "Assertion failed"
    write(stderr, "(A,A)") "File:", file
    write(stderr, "(A,I0)") "Line:", line
    call mpi_abort(MPI_COMM_WORLD, MPIFX_ASSERT_FAILED, aborterror)
    if (aborterror /= 0) then
        write(stderr, "(A)") "Stopping code with 'mpi_abort' did not succeed, trying 'stop' instead"
        stop 1
    end if

  end subroutine assert_failed


#:def getoptarg_template(SUFFIX, TYPE, RANK)

  #:assert RANK >= 0

  subroutine getoptarg_${SUFFIX}$(defarg, arg, optarg)
    ${TYPE}$, intent(in) :: defarg${RANKSUFFIX(RANK)}$
    ${TYPE}$, intent(out) :: arg${RANKSUFFIX(RANK)}$
    ${TYPE}$, intent(in), optional :: optarg${RANKSUFFIX(RANK)}$

    if (present(optarg)) then
      arg = optarg
    else
      arg = defarg
    end if

  end subroutine getoptarg_${SUFFIX}$

#:enddef


#:def setoptarg_template(SUFFIX, TYPE, RANK)

  #:assert RANK >= 0

  subroutine setoptarg_${SUFFIX}$(curval, optval)
    ${TYPE}$, intent(in) :: curval${RANKSUFFIX(RANK)}$
    ${TYPE}$, intent(out), optional :: optval${RANKSUFFIX(RANK)}$

    if (present(optval)) then
      optval = curval
    end if

  end subroutine setoptarg_${SUFFIX}$

#:enddef


#:for TYPE in ALL_TYPES
  #:for RANK in OPT_ARG_RANKS

    #:set SUFFIX = TYPE_ABBREVS[TYPE] + str(RANK)
    #:set FTYPE = FORTRAN_TYPES[TYPE]

    $:getoptarg_template(SUFFIX, FTYPE, RANK)
    $:setoptarg_template(SUFFIX, FTYPE, RANK)

  #:endfor
#:endfor

end module mpifx_helper_module

!> \endcond
