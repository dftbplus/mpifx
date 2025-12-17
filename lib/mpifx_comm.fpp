!> Contains the extended MPI communicator.
module mpifx_comm_module
  use mpi_f08
  use mpifx_helper_module, only : getoptarg, handle_errorflag
  implicit none
  private

  public :: mpifx_comm

  !> MPI communicator with some additional information.
  type mpifx_comm
    integer :: id           !< Communicator id.
    type(mpi_comm) :: comm  !< MPI communicator handle.
    integer :: size         !< Nr. of processes (size).
    integer :: rank         !< Rank of the current process.
    integer :: leadrank     !< Index of the lead node.
    logical :: lead         !< True if current process is the lead (rank == 0).
  contains

    !> Initializes the MPI environment.
    procedure, private :: mpifx_comm_from_id
    procedure, private :: mpifx_comm_from_type

    generic :: init => mpifx_comm_from_id, mpifx_comm_from_type

    !> Creates a new communicator by splitting the old one.
    procedure :: split => mpifx_comm_split

    !> Creates a new communicator by splitting the old one given a split type.
    procedure :: split_type => mpifx_comm_split_type

    !> Frees the communicator. The communicator should not be used after this.
    procedure :: free => mpifx_comm_free

  end type mpifx_comm

contains

  !> Initializes a communicator from mpi_comm type
  !!
  !! \param self  Initialized instance on exit.
  !! \param comm MPI Communicator (default: \c MPI_COMM_WORLD)
  !! \param error  Error flag on return containing the first error occurring
  !!     during the calls mpi_comm_size and mpi_comm_rank.
  !!
  subroutine mpifx_comm_from_type(self, comm, error)
    class(mpifx_comm), intent(out) :: self
    type(mpi_comm), intent(in), optional :: comm
    integer, intent(out), optional :: error

    integer :: error0

    if (present(comm)) then
      self%comm = comm
    else
      self%comm = MPI_COMM_WORLD
    end if
    self%id = self%comm%mpi_val
    call mpi_comm_size(self%comm, self%size, error0)
    call handle_errorflag(error0, "mpi_comm_size() in mpifx_comm_init()", error)
    if (error0 /= 0) then
      return
    end if
    call mpi_comm_rank(self%comm, self%rank, error0)
    call handle_errorflag(error0, "mpi_comm_rank() in mpifx_comm_init()", error)
    if (error0 /= 0) then
      return
    end if
    self%leadrank = 0
    self%lead = (self%rank == self%leadrank)

  end subroutine mpifx_comm_from_type


  !> Initializes a communicator from a numerical id.
  !!
  !! \param self  Initialized instance on exit.
  !! \param commid  Numerical MPI Communicator ID
  !! \param error  Error flag on return containing the first error occurring
  !!     during the calls mpi_comm_size and mpi_comm_rank.
  !!
  subroutine mpifx_comm_from_id(self, commid, error)
    class(mpifx_comm), intent(out) :: self
    integer, intent(in) :: commid
    integer, intent(out), optional :: error

    type(mpi_comm) :: newcomm

    newcomm%mpi_val = commid
    call self%mpifx_comm_from_type(newcomm, error)

  end subroutine mpifx_comm_from_id


  !> Creates a new communicators by splitting the old one.
  !!
  !! \param self  Communicator instance.
  !! \param splitkey  Key for the splitting. Processes invoking the routine
  !!     with the same value for splitkey will be belong to the same
  !!     communicator.
  !! \param rankkey  Is used to determine the rank of the process in its new
  !!     communicator. Processes calling the routine with a higher value will
  !!     have a higher rank in the new communicator.
  !! \param newcomm  New communicator for the given process.
  !! \param error  Optional error code on return.
  !!
  !! Example:
  !!
  !!     program test_split
  !!       use libmpifx_module
  !!       implicit none
  !!
  !!       type(mpifx_comm) :: allproc, groupproc
  !!       integer :: groupsize, mygroup
  !!
  !!       call mpifx_init()
  !!       call allproc%init()
  !!       groupsize = allproc%size / 2
  !!       mygroup = allproc%rank / groupsize
  !!       call allproc%split(mygroup, allproc%rank, groupproc)
  !!       write(*, "(3(A,1X,I0,1X))") "ID:", allproc%rank, "SUBGROUP", &
  !!           & mygroup, "SUBGROUP ID", groupproc%rank
  !!       call mpifx_finalize()
  !!
  !!     end program test_split
  !!
  !! \see MPI documentation (\c MPI_COMM_SPLIT)
  !!
  subroutine mpifx_comm_split(self, splitkey, rankkey, newcomm, error)
    class(mpifx_comm), intent(in) :: self
    integer, intent(in) :: splitkey, rankkey
    class(mpifx_comm), intent(out) :: newcomm
    integer, intent(out), optional :: error

    integer :: error0
    type(mpi_comm) :: newmpicomm

    call mpi_comm_split(self%comm, splitkey, rankkey, newmpicomm, error0)
    call handle_errorflag(error0, "mpi_comm_split() in mpifx_comm_split()", error)
    if (error0 /= 0) then
      return
    end if
    call newcomm%init(newmpicomm, error)

  end subroutine mpifx_comm_split


  !> Creates a new communicator by splitting the old one applying a given split type.
  !!
  !! \param self  Communicator instance.
  !! \param splittype  Determines which ranks to be grouped together. In MPI 3.0,
  !!     this can only be MPI_COMM_TYPE_SHARED grouping all MPI ranks together
  !!     that can share memory (usually on a node).
  !! \param rankkey  Is used to determine the rank of the process in its new
  !!     communicator. Processes calling the routine with a higher value will
  !!     have a higher rank in the new communicator.
  !! \param newcomm  New communicator for the given process.
  !! \param error  Optional error code on return.
  !!
  !! Example:
  !!
  !!     program test_split_type
  !!       use libmpifx_module
  !!       implicit none
  !!
  !!       type(mpifx_comm) :: allproc, splitproc
  !!
  !!       call mpifx_init()
  !!       call allproc%init()
  !!       call allproc%split_type(MPI_COMM_TYPE_SHARED, allproc%rank, splitproc)
  !!       write(*, "(2(A,1X,I0,1X))") "ID:", allproc%rank, "SPLIT ID", splitproc%rank
  !!       call mpifx_finalize()
  !!
  !!     end program test_split_type
  !!
  !! \see MPI documentation (\c MPI_COMM_SPLIT_TYPE)
  !!
  subroutine mpifx_comm_split_type(self, splittype, rankkey, newcomm, error)
    class(mpifx_comm), intent(inout) :: self
    integer, intent(in) :: splittype, rankkey
    class(mpifx_comm), intent(out) :: newcomm
    integer, intent(out), optional :: error

    integer :: error0
    type(mpi_comm) :: newmpicomm

    call mpi_comm_split_type(self%comm, splittype, rankkey, MPI_INFO_NULL, newmpicomm, error0)
    call handle_errorflag(error0, "mpi_comm_split_type() in mpifx_comm_split_type()", error)
    if (error0 /= 0) then
      return
    end if
    call newcomm%init(newmpicomm, error)

  end subroutine mpifx_comm_split_type


  !> Frees the MPI communicator.
  !>
  !> After this call, the passed communicator should not be used any more.
  !>
  !> \param self  Communicator instance.
  !>
  subroutine mpifx_comm_free(self)
    class(mpifx_comm), intent(inout) :: self

    integer :: error

    call mpi_comm_free(self%comm, error)
    self%id = self%comm%mpi_val

  end subroutine mpifx_comm_free


end module mpifx_comm_module
