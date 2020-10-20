#:include 'mpifx.fypp'
#:set TYPES = ALL_TYPES
#:set RANKS = range(MAX_RANK + 1)

!> Contains wrapper for \c MPI_ALLGATHER
module mpifx_allgather_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_allgather

  !> Gathers scalars/arrays on all nodes.
  !!
  !! All functions have the same argument list only differing in the type and
  !! rank of the second and third arguments. The second and third arguments can
  !! be of type integer, real, double precision, complex, double complex and
  !! logical. Their rank can vary from zero (scalars) up to the maximum
  !! rank. Both arguments must be of same type. The third argument must have the
  !! size of the second times the number of processes taking part in the
  !! gathering. The third argument must have either the same rank as the second
  !! one or one rank more. In latter case its last dimension must be of the size
  !! of the number of processes participating in the gathering operation.
  !!
  !! See MPI documentation (mpi_allgather()) for further details.
  !!
  !! Example:
  !!
  !!     program test_gather
  !!       use libmpifx_module
  !!       implicit none
  !!
  !!       type(mpifx_comm) :: mycomm
  !!       integer :: send0
  !!       integer, allocatable :: send1(:)
  !!       integer, allocatable :: recv1(:), recv2(:,:)
  !!       character(100) :: formstr
  !!       character(*), parameter :: label = "(I2.2,'-',I3.3,'|',1X"
  !!
  !!       call mpifx_init()
  !!       call mycomm%init()
  !!
  !!       ! I0 -> I1
  !!       send0 = mycomm%rank * 2
  !!       allocate(recv1(1 * mycomm%size))
  !!       recv1(:) = 0
  !!       write(*, *) mycomm%rank, "Send0 buffer:", send0
  !!       call mpifx_gather(mycomm, send0, recv1)
  !!       write(*, *) mycomm%rank, "Recv1 buffer:", recv1(:)
  !!       deallocate(recv1)
  !!
  !!       ! I1 -> I1
  !!       allocate(send1(2))
  !!       allocate(recv1(size(send1) * mycomm%size))
  !!       recv1(:) = 0
  !!       send1(:) = [ mycomm%rank, mycomm%rank + 1 ]
  !!       write(*, *) "Send1 buffer:", send1(:)
  !!       call mpifx_gather(mycomm, send1, recv1)
  !!       write(*, *) "Recv1 buffer:", recv1
  !!
  !!       ! I1 -> I2
  !!       allocate(recv2(size(send1), mycomm%size))
  !!       recv2(:,:) = 0
  !!       send1(:) = [ mycomm%rank, mycomm%rank + 1 ]
  !!       write(*, *) "Send1 buffer:", send1(:)
  !!       call mpifx_gather(mycomm, send1, recv2)
  !!       write(*, *) "Recv2 buffer:", recv2
  !!
  !!       call mpifx_finalize()
  !!
  !!     end program test_gather
  !!
  interface mpifx_allgather
#:for TYPE in TYPES
  #:for RANK in RANKS
    #:set TYPEABBREV = TYPE_ABBREVS[TYPE]

    #:if RANK > 0
      module procedure mpifx_allgather_${TYPEABBREV}$${RANK}$${TYPEABBREV}$${RANK}$
    #:endif

    #:if RANK < MAX_RANK
      module procedure mpifx_allgather_${TYPEABBREV}$${RANK}$${TYPEABBREV}$${RANK + 1}$
    #:endif

  #:endfor
#:endfor
  end interface mpifx_allgather

contains


#:def mpifx_allgather_dr0_template(SUFFIX, TYPE, MPITYPE, RANK, HASLENGTH)

  #:assert RANK > 0

  !> Gathers results on all processes (type ${SUFFIX}$).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_${SUFFIX}$(mycomm, send, recv, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be sent for gathering.
    ${TYPE}$, intent(in) :: send${RANKSUFFIX(RANK)}$

    !>  Received data.
    ${TYPE}$, intent(out) :: recv${RANKSUFFIX(RANK)}$

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0

    #:set SIZE = 'size(send)'
    #:set COUNT = ('len(send) * ' + SIZE if HASLENGTH else SIZE)

    @:ASSERT(size(recv) == ${SIZE}$ * mycomm%size)
    @:ASSERT(size(recv, dim=${RANK}$) == size(send, dim=${RANK}$) * mycomm%size)

    call mpi_allgather(send, ${COUNT}$, ${MPITYPE}$, recv, ${COUNT}$, ${MPITYPE}$, mycomm%id,&
        & error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_${SUFFIX}$', error)

  end subroutine mpifx_allgather_${SUFFIX}$

#:enddef mpifx_allgather_dr0_template


#:def mpifx_allgather_dr1_template(SUFFIX, TYPE, MPITYPE, RANK, HASLENGTH)

  #:assert RANK >= 0

  !> Gathers results on all processes (type ${SUFFIX}$).
  !!
  !! See mpi_allgather() for further details.
  !!
  subroutine mpifx_allgather_${SUFFIX}$(mycomm, send, recv, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be sent for gathering.
    ${TYPE}$, intent(in) :: send${RANKSUFFIX(RANK)}$

    !>  Received data.
    ${TYPE}$, intent(out) :: recv${RANKSUFFIX(RANK + 1)}$

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0

    #:set SIZE = '1' if RANK == 0 else 'size(send)'
    #:set COUNT = ('len(send) * ' + SIZE if HASLENGTH else SIZE)

    @:ASSERT(size(recv) == ${SIZE}$ * mycomm%size)
    @:ASSERT(size(recv, dim=${RANK + 1}$) == mycomm%size)

    call mpi_allgather(send, ${COUNT}$, ${MPITYPE}$, recv, ${COUNT}$, ${MPITYPE}$,&
        & mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLGATHER in mpifx_allgather_${SUFFIX}$', error)

  end subroutine mpifx_allgather_${SUFFIX}$

#:enddef mpifx_allgather_dr1_template


#:for TYPE in TYPES
  #:for RANK in RANKS

    #:set FTYPE = FORTRAN_TYPES[TYPE]
    #:set MPITYPE = MPI_TYPES[TYPE]
    #:set HASLENGTH = HAS_LENGTH[TYPE]

    #:if RANK > 0
      #:set SUFFIX = TYPE_ABBREVS[TYPE] + str(RANK) + TYPE_ABBREVS[TYPE] + str(RANK)
      $:mpifx_allgather_dr0_template(SUFFIX, FTYPE, MPITYPE, RANK, HASLENGTH)
    #:endif

    #:if RANK < MAX_RANK
      #:set SUFFIX = TYPE_ABBREVS[TYPE] + str(RANK) + TYPE_ABBREVS[TYPE] + str(RANK + 1)
      $:mpifx_allgather_dr1_template(SUFFIX, FTYPE, MPITYPE, RANK, HASLENGTH)
    #:endif

  #:endfor
#:endfor

end module mpifx_allgather_module
