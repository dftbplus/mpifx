#:include 'mpifx.fypp'
#:set TYPES = ALL_TYPES
#:set RANKS = range(1, MAX_RANK + 1)

!> Contains wrapper for \c MPI_SCATTER
module mpifx_scatter_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_scatter

  !> Scatters scalars/arrays on a given node.
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
  !! in the scattering.
  !!
  !! \see MPI documentation (\c MPI_SCATTER)
  !!
  !! Example:
  !!
  !!     program test_scatter
  !!       use libmpifx_module
  !!       implicit none
  !!     
  !!       type(mpifx_comm) :: mycomm
  !!       integer, allocatable :: send1(:), send2(:,:)
  !!       integer :: recv0
  !!       integer, allocatable :: recv1(:)
  !!       integer :: ii
  !!     
  !!       call mpifx_init()
  !!       call mycomm%init()
  !!     
  !!       ! I1 -> I0
  !!       if (mycomm%lead) then
  !!         allocate(send1(mycomm%size))
  !!         send1(:) = [ (ii, ii = 1, size(send1)) ]
  !!         write(*, *) mycomm%rank, "Send1 buffer:", send1
  !!       else
  !!         allocate(send1(0))
  !!       end if
  !!       recv0 = 0
  !!       call mpifx_scatter(mycomm, send1, recv0)
  !!       write(*, *) mycomm%rank, "Recv0 buffer:", recv0
  !!     
  !!       ! I1 -> I1
  !!       if (mycomm%lead) then
  !!         deallocate(send1)
  !!         allocate(send1(2 * mycomm%size))
  !!         send1(:) = [ (ii, ii = 1, size(send1)) ]
  !!         write(*, *)  mycomm%rank, "Send1 buffer:", send1
  !!       end if
  !!       allocate(recv1(2))
  !!       recv1(:) = 0
  !!       call mpifx_scatter(mycomm, send1, recv1)
  !!       write(*, *) mycomm%rank, "Recv1 buffer:", recv1
  !!     
  !!       ! I2 -> I1
  !!       if (mycomm%lead) then
  !!         allocate(send2(2, mycomm%size))
  !!         send2(:,:) = reshape(send1,  [ 2, mycomm%size ])
  !!         write(*, *) mycomm%rank, "Send2 buffer:", send2
  !!       else
  !!         allocate(send2(0,0))
  !!       end if
  !!       recv1(:) = 0
  !!       call mpifx_scatter(mycomm, send2, recv1)
  !!       write(*, *) mycomm%rank, "Recv1 buffer:", recv1
  !!       
  !!       call mpifx_finalize()
  !!       
  !!     end program test_scatter
  !!
  interface mpifx_scatter
#:for TYPE in TYPES
  #:for RANK in RANKS
    #:set TYPEABBREV = TYPE_ABBREVS[TYPE]
    module procedure mpifx_scatter_${TYPEABBREV}$${RANK}$${TYPEABBREV}$${RANK}$
    module procedure mpifx_scatter_${TYPEABBREV}$${RANK}$${TYPEABBREV}$${RANK - 1}$
  #:endfor
#:endfor
  end interface mpifx_scatter

contains

#:def mpifx_scatter_dr0_template(SUFFIX, TYPE, MPITYPE, RANK, HASLENGTH)

  #:assert RANK > 0

  !> Scatters object from one process (type ${SUFFIX}$).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_${SUFFIX}$(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    ${TYPE}$, intent(in) :: send${RANKSUFFIX(RANK)}$
    ${TYPE}$, intent(out) :: recv${RANKSUFFIX(RANK)}$
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    #:set SIZE = 'size(recv)'
    #:set COUNT = ('len(recv) * ' + SIZE if HASLENGTH else SIZE)

    @:ASSERT(.not. mycomm%lead .or. size(send) == size(recv) * mycomm%size)
    @:ASSERT(.not. mycomm%lead&
        & .or. size(send, dim=${RANK}$) == size(recv, dim=${RANK}$) * mycomm%size)
    
    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, ${COUNT}$, ${MPITYPE}$, recv, ${COUNT}$, ${MPITYPE}$, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_${SUFFIX}$", error)
      
  end subroutine mpifx_scatter_${SUFFIX}$

#:enddef mpifx_scatter_dr0_template


#:def mpifx_scatter_dr1_template(SUFFIX, TYPE, MPITYPE, RANK, HASLENGTH)

  #:assert RANK > 0
  
  !> Scatters results on one process (type ${SUFFIX}$).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for scattering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_scatter_${SUFFIX}$(mycomm, send, recv, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    ${TYPE}$, intent(in) :: send${RANKSUFFIX(RANK)}$
    ${TYPE}$, intent(out) :: recv${RANKSUFFIX(RANK - 1)}$
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    #:set SIZE = '1' if RANK == 1 else 'size(recv)'
    #:set COUNT = ('len(recv) * ' + SIZE if HASLENGTH else SIZE)

    @:ASSERT(.not. mycomm%lead .or. size(send) == ${SIZE}$ * mycomm%size)
    @:ASSERT(.not. mycomm%lead .or. size(send, dim=${RANK}$) == mycomm%size)
    #:if HASLENGTH
      @:ASSERT(.not. mycomm%lead .or. len(send) == len(recv))
    #:endif

    call getoptarg(mycomm%leadrank, root0, root)
    call mpi_scatter(send, ${COUNT}$, ${MPITYPE}$, recv, ${COUNT}$, ${MPITYPE}$, root0,&
        & mycomm%id, error0)
    call handle_errorflag(error0, "MPI_SCATTER in mpifx_scatter_${SUFFIX}$", error)

  end subroutine mpifx_scatter_${SUFFIX}$

#:enddef mpifx_scatter_dr1_template


#:for TYPE in TYPES
  #:for RANK in RANKS

    #:set FTYPE = FORTRAN_TYPES[TYPE]
    #:set MPITYPE = MPI_TYPES[TYPE]
    #:set HASLENGTH = HAS_LENGTH[TYPE]

    #:set SUFFIX = TYPE_ABBREVS[TYPE] + str(RANK) + TYPE_ABBREVS[TYPE] + str(RANK)
    $:mpifx_scatter_dr0_template(SUFFIX, FTYPE, MPITYPE, RANK, HASLENGTH)

    #:set SUFFIX = TYPE_ABBREVS[TYPE] + str(RANK) + TYPE_ABBREVS[TYPE] + str(RANK - 1)
    $:mpifx_scatter_dr1_template(SUFFIX, FTYPE, MPITYPE, RANK, HASLENGTH)

  #:endfor
#:endfor

end module mpifx_scatter_module
