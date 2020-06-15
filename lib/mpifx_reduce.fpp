#:include 'mpifx.fypp'
#:set TYPES = NUMERIC_TYPES + LOGICAL_TYPES
#:set RANKS = range(MAX_RANK + 1)

!> Contains wrapper for \c MPI_REDUCE.
module mpifx_reduce_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_reduce, mpifx_reduceip

  !> Reduces a scalar/array on a given node.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second and third arguments. The second and third 
  !! arguments can be of type integer (i), real (s), double precision (d), 
  !! complex (c), double complex (z) or logical (l). Their rank can vary from
  !! zero (scalars) up to the maximum rank. Both arguments must be of same
  !! type and rank.
  !!
  !! \see MPI documentation (\c MPI_REDUCE)
  !!
  !! Example:
  !!
  !!     program test_reduce
  !!       use libmpifx_module
  !!       implicit none
  !!
  !!       integer, parameter :: dp = kind(1.0d0)
  !!
  !!       type(mpifx_comm) :: mycomm
  !!       real(dp) :: valr(3), resvalr(3)
  !!
  !!       call mpifx_init()
  !!       call mycomm%init()
  !!       valr(:) = [ (mycomm%rank + 1) * 1.2_dp, &
  !!           & (mycomm%rank + 1) * 4.3_dp, (mycomm%rank + 1) * 3.8_dp ]
  !!       write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 3, mycomm%rank, &
  !!           & "Value to be operated on:", valr(:)
  !!       call mpifx_reduce(mycomm, valr, resvalr, MPI_PROD)
  !!       write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 4, mycomm%rank, &
  !!           & "Obtained result (prod):", resvalr(:)
  !!       call mpifx_finalize()
  !!       
  !!     end program test_reduce
  !!
  interface mpifx_reduce
#:for TYPE in TYPES
  #:for RANK in RANKS
    module procedure mpifx_reduce_${TYPE_ABBREVS[TYPE]}$${RANK}$
  #:endfor
#:endfor
  end interface mpifx_reduce


  !> Reduces a scalar/array on a given node in place.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second argument. The second argument can be of type
  !! integer (i), real (s), double precision (d), complex (c), double complex
  !! (z) or logical (l). Its rank can vary from zero (scalar) up to the
  !! maximum rank.
  !!
  !! \see MPI documentation (\c MPI_REDUCE)
  !!
  !!
  !! Example:
  !!
  !!     program test_reduceip
  !!       use libmpifx_module
  !!       implicit none
  !!
  !!       integer, parameter :: dp = kind(1.0d0)
  !!
  !!       type(mpifx_comm) :: mycomm
  !!       real(dp) :: resvalr(3)
  !!
  !!       call mpifx_init()
  !!       call mycomm%init()
  !!       resvalr(:) = [ (mycomm%rank + 1) * 1.2_dp, &
  !!           & (mycomm%rank + 1) * 4.3_dp, (mycomm%rank + 1) * 3.8_dp ]
  !!       write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 3, mycomm%rank, &
  !!           & "Value to be operated on:", resvalr(:)
  !!       call mpifx_reduceip(mycomm, resvalr, MPI_PROD)
  !!       write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 4, mycomm%rank, &
  !!           & "Obtained result (prod):", resvalr(:)
  !!       call mpifx_finalize()
  !!       
  !!     end program test_reduceip
  !!  
  interface mpifx_reduceip
#:for TYPE in TYPES
  #:for RANK in RANKS
    module procedure mpifx_reduceip_${TYPE_ABBREVS[TYPE]}$${RANK}$
  #:endfor
#:endfor
  end interface mpifx_reduceip

contains

#:def mpifx_reduce_template(SUFFIX, TYPE, MPITYPE, RANK)

  #:assert RANK >= 0

  !> Reduces on one process (type ${SUFFIX}$).
  !!
  !! \param mycomm  MPI communicator.
  !! \param orig  Quantity to be reduced.
  !! \param reduced  Contains result on exit.
  !! \param reduceop  Reduction operator.
  !! \param root  Root process for the reduced (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduce_${SUFFIX}$(mycomm, orig, reduced, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    ${TYPE}$, intent(in) :: orig${RANKSUFFIX(RANK)}$
    ${TYPE}$, intent(inout) :: reduced${RANKSUFFIX(RANK)}$
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0

    call getoptarg(mycomm%leadrank, root0, root)

    #:set SIZE = '1' if RANK == 0 else 'size(orig)'
    #:set COUNT = SIZE

    call mpi_reduce(orig, reduced, ${COUNT}$, ${MPITYPE}$, reduceop, root0, mycomm%id, error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_${SUFFIX}$", error)
      
  end subroutine mpifx_reduce_${SUFFIX}$

#:enddef mpifx_reduce_template

  
#:def mpifx_reduceip_template(SUFFIX, TYPE, MPITYPE, RANK)

  #:assert RANK >= 0
  
  !> Reduces results on one process (type ${SUFFIX}$).
  !!
  !! \param mycomm  MPI communicator.
  !! \param origred  Quantity to be reduced on input, result on exit
  !! \param reduceop  Reduction reduceop
  !! \param root  Root process for the result (default: mycomm%leadrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_reduceip_${SUFFIX}$(mycomm, origred, reduceop, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    ${TYPE}$, intent(inout) :: origred${RANKSUFFIX(RANK)}$
    integer, intent(in) :: reduceop
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0
    ${TYPE}$ :: dummy

    call getoptarg(mycomm%leadrank, root0, root)

    #:set SIZE = '1' if RANK == 0 else 'size(origred)'
    #:set COUNT = SIZE

    if (mycomm%rank == root0) then
      call mpi_reduce(MPI_IN_PLACE, origred, ${COUNT}$, ${MPITYPE}$, reduceop, root0, mycomm%id,&
          & error0)
    else
      call mpi_reduce(origred, dummy, ${COUNT}$, ${MPITYPE}$, reduceop, root0, mycomm%id, &
          & error0)
    end if
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_reduce_${SUFFIX}$", error)
      
  end subroutine mpifx_reduceip_${SUFFIX}$

#:enddef mpifx_reduceip_template

  
#:for TYPE in TYPES
  #:for RANK in RANKS

    #:set SUFFIX = TYPE_ABBREVS[TYPE] + str(RANK)
    #:set FTYPE = FORTRAN_TYPES[TYPE]
    #:set MPITYPE = MPI_TYPES[TYPE]

    $:mpifx_reduce_template(SUFFIX, FTYPE, MPITYPE, RANK)
    $:mpifx_reduceip_template(SUFFIX, FTYPE, MPITYPE, RANK)

  #:endfor
#:endfor

end module mpifx_reduce_module
