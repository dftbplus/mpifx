include(mpifx_comm.m4)
  
module mpifx_comm_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_comm

  !> MPI communicator with some additional information.
  type mpifx_comm
    integer :: id         !< Communicator id.
    integer :: nproc      !< Nr. of processes (size).
    integer :: iproc      !< Index (rank) of the current process.
    integer :: imaster    !< Index of the master node.
    logical :: master     !< True if current process is the master (rank == 0).
  contains
    !> Initializes the MPI environment.
    procedure :: init => mpifx_comm_init

  end type mpifx_comm

contains

  !> Initializes a communicator to contain all processes.
  !!
  !! \param self  MPI Communicator.
  !! \param error  Error flag on return containing the first error occuring
  !!     during the calls mpi_comm_size and mpi_comm_rank.
  !!
  subroutine mpifx_comm_init(self, error)
    class(mpifx_comm), intent(out) :: self
    integer, intent(out), optional :: error

    integer :: error0

    self%id = default_communicator
    call mpi_comm_size(self%id, self%nproc, error0)
    call handle_errorflag(error0, "mpi_comm_size() in mpifx_comm_init()", error)
    if (error0 /= 0) then
      return
    end if
    call mpi_comm_rank(self%id, self%iproc, error0)
    call handle_errorflag(error0, "mpi_comm_rank() in mpifx_comm_init()", error)
    if (error0 /= 0) then
      return
    end if
    self%imaster = 0
    self%master = (self%iproc == self%imaster)
    
  end subroutine mpifx_comm_init
  
end module mpifx_comm_module
