!> Exports some MPI constants.
!!
!! Note, this module takes the named constants from the MPI module (instead of MPI_F08) in order
!! not to break backwards compatibility. Clients using mpifx should not experience any changes due
!! to the internal change to mpi_f08 almost everywhere.
!!
!! TODO: Before major version is bumped (mpifx 2.0), constants should be separated in a module
!! (e.g. mpifx_mpi_constants), which clients have then to import explicitely (on top of the mpifx
!! library module, which should then not export any constants any more). Those should be the mpi_f08
!! constants, but an additional module providing legacy Fortran 90 mpi-constants
!! (e.g. mpifx_mpi_constants_legacy) should be eventually also considered.
!!
!! \cond HIDDEN
module mpifx_constants_module
  use mpi, only : MPI_MAX, MPI_MIN, MPI_SUM, MPI_PROD, MPI_LAND, MPI_BAND, MPI_LOR, MPI_BOR,&
      & MPI_LXOR, MPI_BXOR, MPI_MAXLOC, MPI_MINLOC, MPI_MODE_NOSTORE, MPI_MODE_NOPUT,&
      & MPI_MODE_NOPRECEDE, MPI_MODE_NOSUCCEED, MPI_THREAD_SINGLE, MPI_THREAD_FUNNELED,&
      & MPI_THREAD_SERIALIZED, MPI_THREAD_MULTIPLE, MPI_COMM_TYPE_SHARED, MPI_ADDRESS_KIND
  implicit none (type, external)
  
  private
  public :: MPI_MAX, MPI_MIN, MPI_SUM, MPI_PROD
  public :: MPI_LAND, MPI_BAND, MPI_LOR, MPI_BOR, MPI_LXOR ,MPI_BXOR
  public :: MPI_MAXLOC, MPI_MINLOC
  public :: MPI_MODE_NOSTORE, MPI_MODE_NOPUT, MPI_MODE_NOPRECEDE, MPI_MODE_NOSUCCEED
  public :: MPI_THREAD_SINGLE, MPI_THREAD_FUNNELED, MPI_THREAD_SERIALIZED, MPI_THREAD_MULTIPLE
  public :: MPI_COMM_TYPE_SHARED
  public :: MPIFX_UNHANDLED_ERROR, MPIFX_ASSERT_FAILED, MPIFX_SIZE_T


  !> Exit code for errors which were not caught due to missing optional arguments
  integer, parameter :: MPIFX_UNHANDLED_ERROR = 1

  !> Exit code for failed assertions
  integer, parameter :: MPIFX_ASSERT_FAILED = 2

  !> Native integer for MPI addresses
  integer, parameter :: MPIFX_SIZE_T = MPI_ADDRESS_KIND

end module mpifx_constants_module

!> \endcond
