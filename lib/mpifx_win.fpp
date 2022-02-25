#:include 'mpifx.fypp'
#:set TYPES = NUMERIC_TYPES

!> Contains routined for MPI shared memory.
module mpifx_win_module
  use mpifx_common_module
  use iso_c_binding, only : c_ptr, c_f_pointer
  implicit none
  private

  public :: mpifx_win

  !> MPI shared memory window with some additional information.
  type mpifx_win
    integer :: id       !< Window id.
    integer :: comm_id  !< Communicator id.
  contains
    !> Initializes an MPI shared memory window.
    generic :: allocate_shared => &
#:for TYPE in TYPES[:-1]
    & mpifx_win_allocate_shared_${TYPE_ABBREVS[TYPE]}$,&
#:endfor
    & mpifx_win_allocate_shared_${TYPE_ABBREVS[TYPES[-1]]}$

#:for TYPE in TYPES
    procedure, private :: mpifx_win_allocate_shared_${TYPE_ABBREVS[TYPE]}$
#:endfor

    !> Locks a shared memory segment.
    procedure :: lock => mpifx_win_lock

    !> Unlocks a shared memory segment.
    procedure :: unlock => mpifx_win_unlock

    !> Synchronizes shared memory across MPI ranks.
    procedure :: sync => mpifx_win_sync

    !> Deallocates memory associated with a shared memory segment.
    procedure :: free => mpifx_win_free

  end type mpifx_win

contains

#:def mpifx_win_allocate_shared_template(SUFFIX, TYPE)

  !> Initialized a window handle and returns a pointer to the address associated with a shared memory segment.
  !!
  !! \param self  Handle of the shared memory window on return.
  !! \param mycomm  MPI communicator.
  !! \param length  Number of elements of type ${TYPE}$ in the shared memory window.
  !! \param shared_data  Pointer to the shared data array of length 'length' on return.
  !! \param error  Optional error code on return.
  !!
  !! \see MPI documentation (\c MPI_WIN_ALLOCATE_SHARED)
  !!
  subroutine mpifx_win_allocate_shared_${SUFFIX}$(self, mycomm, length, shared_data, error)
    class(mpifx_win), intent(out) :: self
    class(mpifx_comm), intent(in) :: mycomm
    integer, intent(in) :: length
    ${TYPE}$, pointer, intent(out) :: shared_data(:)
    integer, intent(out), optional :: error

    integer :: disp_unit, error0, error1
    integer(MPI_ADDRESS_KIND) :: local_length
    type(c_ptr) :: baseptr

    disp_unit = storage_size(shared_data) / 8

    local_length = 0
    if (mycomm%lead) then
      local_length = int(length, kind=MPI_ADDRESS_KIND) * disp_unit
    end if

    call mpi_win_allocate_shared(local_length, disp_unit, MPI_INFO_NULL, mycomm%id, baseptr, self%id, error0)
    call handle_errorflag(error0, "MPI_WIN_ALLOCATE_SHARED in mpifx_win_allocate_shared_${SUFFIX}$", error)

    call mpi_win_shared_query(self%id, 0, local_length, disp_unit, baseptr, error1)
    call handle_errorflag(error1, "MPI_WIN_SHARED_QUERY in mpifx_win_allocate_shared_${SUFFIX}$", error)

    self%comm_id = mycomm%id
    call c_f_pointer(baseptr, shared_data, [length])

  end subroutine mpifx_win_allocate_shared_${SUFFIX}$

#:enddef mpifx_win_allocate_shared_template

  !> Locks a shared memory segment.
  !!
  !! \param self  Handle of the shared memory window.
  !! \param error  Optional error code on return.
  !!
  !! \see MPI documentation (\c MPI_WIN_LOCK_ALL)
  !!
  subroutine mpifx_win_lock(self, error)
    class(mpifx_win), intent(inout) :: self
    integer, intent(out), optional :: error

    integer :: error0

    call mpi_win_lock_all(MPI_MODE_NOCHECK, self%id, error0)
    call handle_errorflag(error0, "MPI_WIN_LOCK_ALL in mpifx_win_lock", error)

  end subroutine mpifx_win_lock

  !> Unlocks a shared memory segment.
  !!
  !! \param self  Handle of the shared memory window.
  !! \param error  Optional error code on return.
  !!
  !! \see MPI documentation (\c MPI_WIN_UNLOCK_ALL)
  !!
  subroutine mpifx_win_unlock(self, error)
    class(mpifx_win), intent(inout) :: self
    integer, intent(out), optional :: error

    integer :: error0

    call mpi_win_unlock_all(self%id, error0)
    call handle_errorflag(error0, "MPI_WIN_UNLOCK_ALL in mpifx_win_unlock", error)

  end subroutine mpifx_win_unlock

  !> Synchronizes shared memory across MPI ranks.
  !!
  !! \param self  Handle of the shared memory window.
  !! \param error  Optional error code on return.
  !!
  !! \see MPI documentation (\c MPI_WIN_SYNC)
  !!
  subroutine mpifx_win_sync(self, error)
    class(mpifx_win), intent(inout) :: self
    integer, intent(out), optional :: error

    integer :: error0, error1

    call mpi_win_sync(self%id, error0)
    call handle_errorflag(error0, "MPI_WIN_SYNC in mpifx_win_sync", error)

    call mpi_barrier(self%comm_id, error1)
    call handle_errorflag(error1, "MPI_BARRIER in mpifx_win_sync", error)

  end subroutine mpifx_win_sync

  !> Deallocates memory associated with a shared memory segment.
  !!
  !! \param self  Handle of the shared memory window.
  !! \param error  Optional error code on return.
  !!
  !! \see MPI documentation (\c MPI_WIN_FREE)
  !!
  subroutine mpifx_win_free(self, error)
    class(mpifx_win), intent(inout) :: self
    integer, intent(out), optional :: error

    integer :: error0

    call mpi_win_free(self%id, error0)
    call handle_errorflag(error0, "MPI_WIN_FREE in mpifx_win_free", error)

  end subroutine mpifx_win_free


#:for TYPE in TYPES
  #:set FTYPE = FORTRAN_TYPES[TYPE]
  #:set SUFFIX = TYPE_ABBREVS[TYPE]

  $:mpifx_win_allocate_shared_template(SUFFIX, FTYPE)

#:endfor

end module mpifx_win_module
