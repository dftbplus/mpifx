!> Exports some MPI constants.
!! \cond HIDDEN
module mpifx_constants_module
  use mpi
  private

  public :: MPI_MAX, MPI_MIN, MPI_SUM, MPI_PROD
  public :: MPI_LAND, MPI_BAND, MPI_LOR, MPI_BOR, MPI_LXOR ,MPI_BXOR
  public :: MPI_MAXLOC, MPI_MINLOC
  public :: MPI_THREAD_SINGLE, MPI_THREAD_FUNNELED, MPI_THREAD_SERIALIZED, MPI_THREAD_MULTIPLE
  public :: MPI_COMM_TYPE_SHARED
  public :: MPIFX_UNHANDLED_ERROR, MPIFX_ASSERT_FAILED


  !> Exit code for errors which were not caught due to missing optional arguments
  integer, parameter :: MPIFX_UNHANDLED_ERROR = 1

  !> Exit code for failed assertions
  integer, parameter :: MPIFX_ASSERT_FAILED = 2

end module mpifx_constants_module

!> \endcond
