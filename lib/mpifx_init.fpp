!> Contains wrapper for \c MPI_INIT.
module mpifx_init_module
  use mpifx_common_module
  use mpifx_constants_module
  implicit none
  private

  public :: mpifx_init, mpifx_init_thread

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

  !> Initializes a threaded MPI environment.
  !!
  !! \param requiredThreading  Threading support required (MPI_THREAD_SINGLE, MPI_THREAD_FUNNELED,
  !!     MPI_THREAD_SERIALIZED, MPI_THREAD_MULTIPLE)
  !! \param proviedeThreading  Threading level provided by the MPI-framework. If not present and
  !!     the framework offers a lower support than required, the routine stops program execution.
  !! \param error Error code on return. If not present and error code would have been non-zero,
  !!     routine aborts program execution.
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
  !!       call mpifx_init_thread(MPI_THREAD_FUNNELED)
  !!       call mycomm%init()
  !!       :
  !!       call mpifx_finalize()
  !!
  !!     end program test_mpifx
  !!

   subroutine mpifx_init_thread(requiredThreading, providedThreading, error)
    integer, intent(in) :: requiredThreading
    integer, intent(out), optional :: providedThreading
    integer, intent(out), optional :: error

    integer :: error0, providedThreading0

    call mpi_init_thread(requiredThreading, providedThreading0, error0)
    if (present(providedThreading)) then
      providedThreading = providedThreading0
    elseif (providedThreading0 < requiredThreading) then
      write(*, "(A,I0,A,I0,A)") "Error: Provided threading model (", providedThreading0,&
          & ") is less than required threading model (", requiredThreading, ")"
      call mpi_abort(MPI_COMM_WORLD, MPIFX_UNHANDLED_ERROR, error0)
    end if
    call handle_errorflag(error0, "Error: mpi_init_thread in mpifx_init_thread()", error)

  end subroutine mpifx_init_thread


end module mpifx_init_module
