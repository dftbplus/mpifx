#:include 'mpifx.fypp'
#:set TYPES = ALL_TYPES
#:set RANKS = range(1, MAX_RANK + 1)

!> Contains wrapper for \c MPI_SCATTER
module mpifx_scatterv_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_scatterv

  !> scatters scalars/arrays of different lengths from a given node.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second and third arguments. The second and third
  !! arguments can be of type integer (i), real (s), double precision (d),
  !! complex (c), double complex (z) and logical (l). Their rank can vary from
  !! zero (scalars) up to the maximum rank. Both arguments must be of same
  !! type. The second argument must have the size of the third times the number
  !! of processes taking part in the scattering. The second argument must have
  !! either the same rank as the third one or one rank more. In latter case
  !! the last dimension of it must be of the size of the number of processes
  !! in the scatterving.
  !!
  !! \see MPI documentation (\c MPI_scatterv)
  !!
  !! Example:
  !!
  !!     program test_scatterv
  !!       use libmpifx_module
  !!       implicit none
  !!
  !!       type(mpifx_comm) :: mycomm
  !!       integer, allocatable :: send1(:)
  !!       integer, allocatable :: recv1(:)
  !!       integer, allocatable :: sendcounts(:)
  !!       integer :: ii, nsend
  !!
  !!       call mpifx_init()
  !!       call mycomm%init()
  !!
  !!       ! I1 -> I1
  !!       allocate(recv1(mycomm%rank+1))
  !!       recv1 = 0
  !!       if (mycomm%lead) then
  !!         ! send1 size is 1+2+3+...+mycomm%size
  !!         nsend = mycomm%size*(mycomm%size+1)/2
  !!         allocate(send1(nsend))
  !!         do ii = 1, nsend
  !!           send1(ii) = ii
  !!         end do
  !!         allocate(sendcounts(mycomm%size))
  !!         do ii = 1, mycomm%size
  !!           sendcounts(ii) = ii
  !!         end do
  !!       else
  !!         allocate(send1(0))
  !!       end if
  !!
  !!       if (mycomm%lead) then
  !!         write(*, *) mycomm%rank, "Send1 buffer:", send1(:)
  !!       end if
  !!       call mpifx_scatterv(mycomm, send1, sendcounts, recv1)
  !!       write(*, *) mycomm%rank, "Recv1 buffer:", recv1
  !!
  !!       call mpifx_finalize()
  !!
  !!     end program test_scatterv
  !!
  interface mpifx_scatterv
#:for TYPE in TYPES
  #:for RANK in RANKS
    #:set TYPEABBREV = TYPE_ABBREVS[TYPE]
    module procedure mpifx_scatterv_${TYPEABBREV}$${RANK}$${TYPEABBREV}$${RANK}$
    module procedure mpifx_scatterv_${TYPEABBREV}$${RANK}$${TYPEABBREV}$${RANK - 1}$
  #:endfor
#:endfor
  end interface mpifx_scatterv

contains

#:def mpifx_scatterv_dr0_template(SUFFIX, TYPE, MPITYPE, RANK, HASLENGTH)

  #:assert RANK > 0

  !> scatters object of variable length from one process (type ${SUFFIX}$).
  !!
  !! \param mycomm MPI communicator.
  !! \param send Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv Received data on receive node (undefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root Root process for the result (default: mycomm%leadrank)
  !! \param error Error code on exit.
  !!
  subroutine mpifx_scatterv_${SUFFIX}$(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    ${TYPE}$, intent(in) :: send${RANKSUFFIX(RANK)}$
    integer, intent(in) :: sendcounts(:)
    ${TYPE}$, intent(out) :: recv${RANKSUFFIX(RANK)}$
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)

    #:set SIZE = 'size(recv)'
    #:set COUNT = ('len(recv) * ' + SIZE if HASLENGTH else SIZE)

    @:ASSERT(.not. mycomm%lead .or. size(send) == size(recv) * mycomm%size)
    @:ASSERT(.not. mycomm%lead&
        & .or. size(send, dim=${RANK}$) == size(recv, dim=${RANK}$) * mycomm%size)
    
    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then
        @:ASSERT(size(displs) == mycomm%size)
        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if
    call mpi_scatterv(send, sendcounts, displs0, ${MPITYPE}$, recv, ${SIZE}$, ${MPITYPE}$, root0,&
        & mycomm%id, error0)

    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_${SUFFIX}$", error)
      
  end subroutine mpifx_scatterv_${SUFFIX}$

#:enddef mpifx_scatterv_dr0_template


#:def mpifx_scatterv_dr1_template(SUFFIX, TYPE, MPITYPE, RANK, HASLENGTH)

  #:assert RANK > 0
  
  !> Scatter results from one process (type ${SUFFIX}$).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param sendcounts Counts of sent data from each process
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param displs Entry i specifies where to take data to send to rank i
  !!        (default: computed from sendcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatterv_${SUFFIX}$(mycomm, send, sendcounts, recv, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    ${TYPE}$, intent(in) :: send${RANKSUFFIX(RANK)}$
    integer, intent(in) :: sendcounts(:)
    ${TYPE}$, intent(out) :: recv${RANKSUFFIX(RANK - 1)}$
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii
    integer, allocatable :: displs0(:)

    #:set SIZE = '1' if RANK == 1 else 'size(recv)'
    #:set COUNT = ('len(recv) * ' + SIZE if HASLENGTH else SIZE)

    @:ASSERT(.not. mycomm%lead .or. size(send) == ${SIZE}$ * mycomm%size)
    @:ASSERT(.not. mycomm%lead .or. size(send, dim=${RANK}$) == mycomm%size)
    #:if HASLENGTH
    @:ASSERT(.not. mycomm%lead .or. len(send) == len(recv))
    #:endif

    call getoptarg(mycomm%leadrank, root0, root)
    if (mycomm%rank == root0) then
      if (present(displs)) then
        @:ASSERT(size(displs) == mycomm%size)
        allocate(displs0(mycomm%size))
        displs0(:) = displs
      else
        allocate(displs0(mycomm%size))
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
        end do
      end if
    end if

    call mpi_scatterv(send, sendcounts, displs0, ${MPITYPE}$, recv, ${COUNT}$, ${MPITYPE}$, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatterv_${SUFFIX}$", error)

  end subroutine mpifx_scatterv_${SUFFIX}$

#:enddef mpifx_scatterv_dr1_template


#:for TYPE in TYPES
  #:for RANK in RANKS

    #:set FTYPE = FORTRAN_TYPES[TYPE]
    #:set MPITYPE = MPI_TYPES[TYPE]
    #:set HASLENGTH = HAS_LENGTH[TYPE]

    #:set SUFFIX = TYPE_ABBREVS[TYPE] + str(RANK) + TYPE_ABBREVS[TYPE] + str(RANK)
    $:mpifx_scatterv_dr0_template(SUFFIX, FTYPE, MPITYPE, RANK, HASLENGTH)

    #:set SUFFIX = TYPE_ABBREVS[TYPE] + str(RANK) + TYPE_ABBREVS[TYPE] + str(RANK - 1)
    $:mpifx_scatterv_dr1_template(SUFFIX, FTYPE, MPITYPE, RANK, HASLENGTH)

  #:endfor
#:endfor

end module mpifx_scatterv_module
