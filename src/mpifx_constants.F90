include(mpifx_constants.m4)

!> Exports some MPI constants.
!! \cond HIDDEN
module mpifx_constants_module
  use mpi
  private

  public :: MPI_MAX, MPI_MIN, MPI_SUM, MPI_PROD
  public :: MPI_LAND, MPI_BAND, MPI_LOR, MPI_BOR, MPI_LXOR ,MPI_BXOR
  public :: MPI_MAXLOC, MPI_MINLOC
  public :: MPI_THREAD_SINGLE, MPI_THREAD_FUNNELED, MPI_THREAD_SERIALIZED, MPI_THREAD_MULTIPLE 
  
end module mpifx_constants_module

!> \endcond
