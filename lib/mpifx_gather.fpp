#:include 'mpifx.fypp'
#:set TYPES = ALL_TYPES
#:set RANKS = range(MAX_RANK + 1)

!> Contains wrapper for \c MPI_GATHER
module mpifx_gather_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_gather

  !> Gathers scalars/arrays on a given node.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second and third arguments. The second and third
  !! arguments can be of type integer (i), real (s), double precision (d),
  !! complex (c), double complex (z) and logical (l). Their rank can vary from
  !! zero (scalars) up to the maximum rank. Both arguments must be of same
  !! type. The third argument must have the size of the second times the number
  !! of processes taking part in the gathering. The third argument must have
  !! either the same rank as the second one or one rank more. In latter case
  !! the last dimension of it must be of the size of the number of processes
  !! in the gathering.
  !!
  !! \see MPI documentation (\c MPI_GATHER)
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
  !!       send0 = mycomm%rank * 2    ! Arbitrary number to send
  !!       if (mycomm%lead) then
  !!         allocate(recv1(1 * mycomm%size))
  !!         recv1(:) = 0
  !!       else
  !!         allocate(recv1(0))
  !!       end if
  !!       write(*, *) mycomm%rank, "Send0 buffer:", send0
  !!       call mpifx_gather(mycomm, send0, recv1)
  !!       if (mycomm%lead) then
  !!         write(*, *) mycomm%rank, "Recv1 buffer:", recv1(:)
  !!       end if
  !!       deallocate(recv1)
  !!     
  !!       ! I1 -> I1
  !!       allocate(send1(2))
  !!       send1(:) = [ mycomm%rank, mycomm%rank + 1 ]  ! Arbitrary numbers
  !!       if (mycomm%lead) then
  !!         allocate(recv1(size(send1) * mycomm%size))
  !!         recv1(:) = 0
  !!       else
  !!         allocate(recv1(0))
  !!       end if
  !!       write(*, *) mycomm%rank, "Send1 buffer:", send1(:)
  !!       call mpifx_gather(mycomm, send1, recv1)
  !!       if (mycomm%lead) then
  !!         write(*, *) mycomm%rank, "Recv1 buffer:", recv1
  !!       end if
  !!     
  !!       ! I1 -> I2
  !!       send1(:) = [ mycomm%rank, mycomm%rank + 1 ]
  !!       if (mycomm%lead) then
  !!         allocate(recv2(size(send1), mycomm%size))
  !!         recv2(:,:) = 0
  !!       end if
  !!       write(*, *) mycomm%rank, "Send1 buffer:", send1(:)
  !!       call mpifx_gather(mycomm, send1, recv2)
  !!       if (mycomm%lead) then
  !!         write(*, *) mycomm%rank, "Recv2 buffer:", recv2
  !!       end if
  !!       
  !!       call mpifx_finalize()
  !!       
  !!     end program test_gather
  !!
  interface mpifx_gather
#:for TYPE in TYPES
  #:for RANK in RANKS
    #:set TYPEABBREV = TYPE_ABBREVS[TYPE]
    #:if RANK > 0
      module procedure mpifx_gather_${TYPEABBREV}$${RANK}$${TYPEABBREV}$${RANK}$
    #:endif
    #:if RANK < MAX_RANK
      module procedure mpifx_gather_${TYPEABBREV}$${RANK}$${TYPEABBREV}$${RANK + 1}$
    #:endif
  #:endfor
#:endfor
  end interface mpifx_gather
  
contains

#:def mpifx_gather_dr0_template(SUFFIX, TYPE, MPITYPE, RANK, HASLENGTH)

  #:assert RANK > 0

  !> Gathers results on one process (type ${SUFFIX}$).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_${SUFFIX}$(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    ${TYPE}$, intent(in) :: send${RANKSUFFIX(RANK)}$
    ${TYPE}$, intent(out) :: recv${RANKSUFFIX(RANK)}$
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    #:set SIZE = 'size(send)'
    #:set COUNT = ('len(send) * ' + SIZE if HASLENGTH else SIZE)

    @:ASSERT(.not. mycomm%lead .or. size(recv) == size(send) * mycomm%size)
    @:ASSERT(.not. mycomm%lead .or.&
        & size(recv, dim=${RANK}$) == size(send, dim=${RANK}$) * mycomm%size)

    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, ${COUNT}$, ${MPITYPE}$, recv, ${COUNT}$, ${MPITYPE}$, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_${SUFFIX}$", error)
    
  end subroutine mpifx_gather_${SUFFIX}$

#:enddef mpifx_gather_dr0_template


#:def mpifx_gather_dr1_template(SUFFIX, TYPE, MPITYPE, RANK, HASLENGTH)

  #:assert RANK >= 0

  !> Gathers results on one process (type ${SUFFIX}$).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gather_${SUFFIX}$(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    ${TYPE}$, intent(in) :: send${RANKSUFFIX(RANK)}$
    ${TYPE}$, intent(out) :: recv${RANKSUFFIX(RANK + 1)}$
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    #:set SIZE = '1' if RANK == 0 else 'size(send)'
    #:set COUNT = ('len(send) * ' + SIZE if HASLENGTH else SIZE)

    @:ASSERT(.not. mycomm%lead .or. size(recv) == ${SIZE}$ * mycomm%size)
    @:ASSERT(.not. mycomm%lead .or. size(recv, dim=${RANK + 1}$) == mycomm%size)

    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_gather(send, ${SIZE}$, ${MPITYPE}$, recv, ${SIZE}$, ${MPITYPE}$, root0, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_GATHER in mpifx_gather_${SUFFIX}$", error)

  end subroutine mpifx_gather_${SUFFIX}$

#:enddef mpifx_gather_dr1_template


#:for TYPE in TYPES
  #:for RANK in RANKS

    #:set FTYPE = FORTRAN_TYPES[TYPE]
    #:set MPITYPE = MPI_TYPES[TYPE]
    #:set HASLENGTH = HAS_LENGTH[TYPE]

    #:if RANK > 0
      #:set SUFFIX = TYPE_ABBREVS[TYPE] + str(RANK) + TYPE_ABBREVS[TYPE] + str(RANK)
      $:mpifx_gather_dr0_template(SUFFIX, FTYPE, MPITYPE, RANK, HASLENGTH)
    #:endif

    #:if RANK < MAX_RANK
      #:set SUFFIX = TYPE_ABBREVS[TYPE] + str(RANK) + TYPE_ABBREVS[TYPE] + str(RANK + 1)
      $:mpifx_gather_dr1_template(SUFFIX, FTYPE, MPITYPE, RANK, HASLENGTH)
    #:endif

  #:endfor
#:endfor


end module mpifx_gather_module
