#:include 'mpifx.fypp'
#:set TYPES = NUMERIC_TYPES

!> Contains utilities for handling MPI shared memory
module mpifx_shared_memory_module
  use mpifx_common_module
  use iso_c_binding, only : c_ptr, c_f_pointer
  implicit none
  private

  public :: mpifx_allocate_shared, mpifx_free_shared, mpifx_lock_shared, mpifx_unlock_shared, mpifx_sync_shared

  interface mpifx_allocate_shared
#:for TYPE in TYPES
  #:set TYPEABBREV = TYPE_ABBREVS[TYPE]
  module procedure mpifx_allocate_shared_${TYPEABBREV}$
#:endfor
  end interface mpifx_allocate_shared

contains

#:def mpifx_allocate_shared_template(SUFFIX, TYPE)

  !> Returns a window handle and a pointer to the address associated with a shared memory segment.
  !!
  !! \param mycomm  MPI communicator.
  !! \param length  Number of elements of type ${TYPE}$ in the shared memory window.
  !! \param win  Handle of the shared memory window on return.
  !! \param shared_data  Pointer to the shared data array of length 'length' on return.
  !! \param error  Optional error code on return.
  !!
  !! \see MPI documentation (\c MPI_WIN_ALLOCATE_SHARED)
  !!
  subroutine mpifx_allocate_shared_${SUFFIX}$(mycomm, length, win, shared_data, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: length
    integer, intent(out) :: win
    ${TYPE}$, pointer, intent(out) :: shared_data(:)
    integer, intent(out), optional :: error

    integer :: disp_unit, error0, error1
    integer(MPI_ADDRESS_KIND) :: local_length
    type(c_ptr) :: baseptr

    disp_unit = kind(shared_data)

    local_length = 0
    if (mycomm%lead) then
      local_length = length * disp_unit
    end if

    call mpi_win_allocate_shared(local_length, disp_unit, MPI_INFO_NULL, mycomm%id, baseptr, win, error0)
    call handle_errorflag(error0, "MPI_WIN_ALLOCATE_SHARED in mpifx_allocate_shared_${SUFFIX}$", error)

    call mpi_win_shared_query(win, 0, local_length, disp_unit, baseptr, error1)
    call handle_errorflag(error1, "MPI_WIN_SHARED_QUERY in mpifx_allocate_shared_${SUFFIX}$", error)

    call c_f_pointer(baseptr, shared_data, [length])

  end subroutine mpifx_allocate_shared_${SUFFIX}$

#:enddef mpifx_allocate_shared_template

  !> Deallocates a memory associated with a shared memory segment.
  !!
  !! \param win  Handle of the shared memory window.
  !! \param error  Optional error code on return.
  !!
  !! \see MPI documentation (\c MPI_WIN_FREE)
  !!
  subroutine mpifx_free_shared(win, error)
    integer, intent(inout) :: win
    integer, intent(out), optional :: error

    integer :: error0

    call mpi_win_free(win, error0)
    call handle_errorflag(error0, "MPI_WIN_FREE in mpifx_free_shared", error)

  end subroutine mpifx_free_shared

  !> Locks a shared memory segment.
  !!
  !! \param win  Handle of the shared memory window.
  !! \param error  Optional error code on return.
  !!
  !! \see MPI documentation (\c MPI_WIN_LOCK_ALL)
  !!
  subroutine mpifx_lock_shared(win, error)
    integer, intent(inout) :: win
    integer, intent(out), optional :: error

    integer :: error0

    call mpi_win_lock_all(MPI_MODE_NOCHECK, win, error0)
    call handle_errorflag(error0, "MPI_WIN_LOCK_ALL in mpifx_lock_shared", error)

  end subroutine mpifx_lock_shared

  !> Unlocks a shared memory segment.
  !!
  !! \param win  Handle of the shared memory window.
  !! \param error  Optional error code on return.
  !!
  !! \see MPI documentation (\c MPI_WIN_UNLOCK_ALL)
  !!
  subroutine mpifx_unlock_shared(win, error)
    integer, intent(inout) :: win
    integer, intent(out), optional :: error

    integer :: error0

    call mpi_win_unlock_all(win, error0)
    call handle_errorflag(error0, "MPI_WIN_UNLOCK_ALL in mpifx_unlock_shared", error)

  end subroutine mpifx_unlock_shared

  !> Synchronizes shared memory across MPI ranks.
  !!
  !! \param mycomm  MPI communicator.
  !! \param win  Handle of the shared memory window.
  !! \param error  Optional error code on return.
  !!
  !! \see MPI documentation (\c MPI_WIN_SYNC)
  !!
  subroutine mpifx_sync_shared(mycomm, win, error)
    type(mpifx_comm), intent(in) :: mycomm
    integer, intent(inout) :: win
    integer, intent(out), optional :: error

    integer :: error0, error1

    call mpi_win_sync(win, error0)
    call handle_errorflag(error0, "MPI_WIN_SYNC in mpifx_sync_shared", error)

    call mpi_barrier(mycomm%id, error1)
    call handle_errorflag(error1, "MPI_BARRIER in mpifx_sync_shared", error)

  end subroutine mpifx_sync_shared


#:for TYPE in TYPES
  #:set FTYPE = FORTRAN_TYPES[TYPE]
  #:set SUFFIX = TYPE_ABBREVS[TYPE]

  $:mpifx_allocate_shared_template(SUFFIX, FTYPE)

#:endfor

end module mpifx_shared_memory_module
