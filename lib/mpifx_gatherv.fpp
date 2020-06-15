#:include 'mpifx.fypp'
#:set TYPES = ALL_TYPES
#:set RANKS = range(1, MAX_RANK + 1)

#! ************************************************************************
#! *** mpifx_gatherv
#! ************************************************************************


#:def mpifx_gatherv_dr0_template(SUFFIX, TYPE, RANK, MPI_TYPE)

  !> Gathers results of variable length on one process (type ${SUFFIX}$).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_${SUFFIX}$(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    ${TYPE}$, intent(in) :: send${RANKSUFFIX(RANK)}$
    ${TYPE}$, intent(out) :: recv${RANKSUFFIX(RANK)}$
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

    @:inoptflags(root0, root, mycomm%leadrank)

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then
        @:ASSERT(size(displs) == mycomm%size)
        displs0 = displs
        locLast = maxloc(displs0)
        @:ASSERT(size(recv) >= displs0(locLast(1)) + recvcounts(locLast(1)))
        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do
        @:ASSERT(sum(recvcounts) == size(recv))
      end if
    end if

    call mpi_gatherv(send, size(send), ${MPI_TYPE}$, recv, recvcounts, displs0, &
        & ${MPI_TYPE}$, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_${SUFFIX}$", error)

  end subroutine mpifx_gatherv_${SUFFIX}$

#:enddef



#:def mpifx_gatherv_dr1_template(SUFFIX, TYPE, SEND_RANK, SEND_SIZE, RECV_RANK, MPI_TYPE)

  !> Gathers results on one process (type ${SUFFIX}$).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_${SUFFIX}$(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    ${TYPE}$, intent(in) :: send${RANKSUFFIX(SEND_RANK)}$
    ${TYPE}$, intent(out) :: recv${RANKSUFFIX(RECV_RANK)}$
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: ii, root0, error0
    integer, allocatable :: displs0(:)

    @:inoptflags(root0, root, mycomm%leadrank)

    if (mycomm%rank == root0) then
      @:ASSERT(size(recv) == sum(recvcounts))
      @:ASSERT(size(recv, dim=${RECV_RANK}$) == mycomm%size)
      allocate(displs0(mycomm%size))
      if (present(displs)) then
        @:ASSERT(size(displs) == mycomm%size)
        displs0 = displs
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do
      end if
    end if

    call mpi_gatherv(send, ${SEND_SIZE}$, ${MPI_TYPE}$, recv, recvcounts, displs0, &
         & ${MPI_TYPE}$,  root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_${SUFFIX}$", error)

  end subroutine mpifx_gatherv_${SUFFIX}$

#:enddef


!> Contains wrapper for \c MPI_gatherv
module mpifx_gatherv_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_gatherv

  !> Gathers scalars/arrays of different lengths on a given node.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second and third arguments. The second and third
  !! arguments can be of type integer (i), real (s), double precision (d),
  !! complex (c), double complex (z) and logical (l). Their rank can vary from
  !! zero (scalars) up to the maximum rank. Both arguments must be of same
  !! type. The third argument must have the size of the second times the number
  !! of processes taking part in the gathering. The fourth argument must be
  !! an array of integers corresponding to the array sizes received from each
  !! processor. The displacements at which to place the incoming data can be
  !! given as an optional argument. By default they are computed from recvcounts,
  !! assuming ordering with processor rank.
  !!
  !! \see MPI documentation (\c MPI_gatherv)
  !!
  !! Example:
  !!
  !!     program test_gatherv
  !!       use libmpifx_module
  !!       implicit none
  !!
  !!       type(mpifx_comm) :: mycomm
  !!       real, allocatable :: send1(:)
  !!       real, allocatable :: recv1(:)
  !!       integer, allocatable :: recvcounts(:)
  !!       integer :: ii, nrecv
  !!       character(100) :: formstr
  !!       character(*), parameter :: label = "(I2.2,'-',I3.3,'|',1X"
  !!
  !!       call mpifx_init()
  !!       call mycomm%init()
  !!
  !!       ! I1 -> I1
  !!       allocate(send1(mycomm%rank+1))
  !!       send1 = 1.0*mycomm%rank
  !!       if (mycomm%lead) then
  !!         ! recv1 size is 1+2+3+...+mycomm%size
  !!         nrecv = mycomm%size*(mycomm%size+1)/2
  !!         allocate(recv1(nrecv))
  !!         recv1(:) = 0
  !!         allocate(recvcounts(mycomm%size))
  !!         do ii = 1, mycomm%size
  !!           recvcounts(ii) = ii
  !!         end do
  !!       else
  !!         allocate(recv1(0))
  !!       end if
  !!
  !!       write(*, *) mycomm%rank, "Send1 buffer:", send1(:)
  !!       call mpifx_gatherv(mycomm, send1, recv1, recvcounts)
  !!       if (mycomm%lead) then
  !!         write(*, *) mycomm%rank, "Recv1 buffer:", recv1
  !!       end if
  !!
  !!       call mpifx_finalize()
  !!
  !!     end program test_gatherv
  !!
  interface mpifx_gatherv
    #:for TYPE in TYPES
      #:for RANK in RANKS
        #:set TYPEABBREV = TYPE_ABBREVS[TYPE]
        module procedure mpifx_gatherv_${TYPEABBREV}$${RANK}$${TYPEABBREV}$${RANK}$
      #:endfor
      module procedure mpifx_gatherv_${TYPEABBREV}$0${TYPEABBREV}$1
    #:endfor
  end interface mpifx_gatherv


contains

  #:for TYPE in TYPES

    #:set FTYPE = FORTRAN_TYPES[TYPE]
    #:set MPITYPE = MPI_TYPES[TYPE]

    #:for RANK in RANKS
      #:set SUFFIX = TYPE_ABBREVS[TYPE] + str(RANK) + TYPE_ABBREVS[TYPE] + str(RANK)
      $:mpifx_gatherv_dr0_template(SUFFIX, FTYPE, RANK, MPITYPE)
    #:endfor

    #:set SUFFIX = TYPE_ABBREVS[TYPE] + str(0) + TYPE_ABBREVS[TYPE] + str(1)
    $:mpifx_gatherv_dr1_template(SUFFIX, FTYPE, 0, 1, 1, MPITYPE)

  #:endfor

end module mpifx_gatherv_module
