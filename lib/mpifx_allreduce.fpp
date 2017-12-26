#:include 'mpifx.fypp'
#:set TYPES = NUMERIC_TYPES + LOGICAL_TYPES
#:set RANKS = range(MAX_RANK + 1)

!> Contains wrapper for \c MPI_ALLREDUCE.
module mpifx_allreduce_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_allreduce, mpifx_allreduceip

  !> Reduces a scalar/array on all nodes.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second and third arguments. The second and third
  !! arguments can be of type integer (i), real (s), double precision (d),
  !! complex (c), double complex (z) and logical (l). Their rank can vary from
  !! zero (scalars) up to the maximum rank. Both arguments must be of same
  !! type and rank.
  !!
  !! \see MPI documentation (\c MPI_ALLREDUCE)
  !!
  !! Example:
  !!
  !!     program test_allreduce
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
  !!       call mpifx_allreduce(mycomm, valr, resvalr, MPI_PROD)
  !!       write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 4, mycomm%rank, &
  !!           & "Obtained result (prod):", resvalr(:)
  !!       call mpifx_finalize()
  !!
  !!     end program test_allreduce
  !!
  interface mpifx_allreduce
#:for TYPE in TYPES
  #:for RANK in RANKS
    module procedure mpifx_allreduce_${TYPE_ABBREVS[TYPE]}$${RANK}$
  #:endfor
#:endfor
  end interface mpifx_allreduce


  !> Reduces a scalar/array on all nodes in place.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second argument. The second argument can be of type
  !! integer (i), real (s), double precision (d), complex (c), double complex
  !! (z) or logical (l). Its rank can vary from zero (scalar) up to the
  !! maximum rank.
  !!
  !! \see MPI documentation (\c MPI_ALLREDUCE)
  !!
  !!
  !! Example:
  !!
  !!     program test_allreduceip
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
  !!       call mpifx_allreduceip(mycomm, resvalr, MPI_PROD)
  !!       write(*, "(I2.2,'-',I3.3,'|',1X,A,3F8.2)") 4, mycomm%rank, &
  !!           & "Obtained result (prod):", resvalr(:)
  !!       call mpifx_finalize()
  !!       
  !!     end program test_allreduceip
  !!
  interface mpifx_allreduceip
#:for TYPE in TYPES
  #:for RANK in RANKS
    module procedure mpifx_allreduceip_${TYPE_ABBREVS[TYPE]}$${RANK}$
  #:endfor
#:endfor
  end interface mpifx_allreduceip

contains

#:def mpifx_allreduce_template(SUFFIX, TYPE, MPITYPE, RANK)

  #:assert RANK >= 0

  !> Reduces operand on all processes (type $1).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduce_${SUFFIX}$(mycomm, orig, reduced, reductionop, error)

    !> MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !> Quantity to be reduced.
    ${TYPE}$, intent(in) :: orig${RANKSUFFIX(RANK)}$

    !> Contains result on exit.
    ${TYPE}$, intent(inout) :: reduced${RANKSUFFIX(RANK)}$

    !>  Reduction operator
    integer, intent(in) :: reductionop

    !>  Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0

    #:if RANK > 0
      @:ASSERT(size(orig) == size(reduced))
    #:endif

    #:set SIZE = '1' if RANK == 0 else 'size(orig)'
    #:set COUNT = SIZE

    call mpi_allreduce(orig, reduced, ${COUNT}$, ${MPITYPE}$, reductionop, mycomm%id, error0)
    call handle_errorflag(error0, 'MPI_ALLREDUCE in mpifx_allreduce_${SUFFIX}$', error)

  end subroutine mpifx_allreduce_${SUFFIX}$

#:enddef mpifx_allreduce_template


#:def mpifx_allreduceip_template(SUFFIX, TYPE, MPITYPE, RANK)

  #:assert RANK >= 0

  !> Reduces operand on all processes (type ${SUFFIX}$).
  !!
  !! See MPI documentation (mpi_allreduce()) for further details.
  !!
  subroutine mpifx_allreduceip_${SUFFIX}$(mycomm, origreduced, reductionop, error)

    !>  MPI communicator.
    type(mpifx_comm), intent(in) :: mycomm

    !>  Quantity to be reduced on input, reduced on exit.
    ${TYPE}$, intent(inout) :: origreduced${RANKSUFFIX(RANK)}$

    !> Reduction operator.
    integer, intent(in) :: reductionop

    !> Error code on exit.
    integer, intent(out), optional :: error

    integer :: error0

    #:set SIZE = '1' if RANK == 0 else 'size(origreduced)'
    #:set COUNT = SIZE

    call mpi_allreduce(MPI_IN_PLACE, origreduced, ${COUNT}$, ${MPITYPE}$, reductionop, mycomm%id,&
        & error0)
    call handle_errorflag(error0, "MPI_REDUCE in mpifx_allreduceip_${SUFFIX}$", error)

  end subroutine mpifx_allreduceip_${SUFFIX}$

#:enddef mpifx_allreduceip_template


#:for TYPE in TYPES
  #:for RANK in RANKS

    #:set SUFFIX = TYPE_ABBREVS[TYPE] + str(RANK)
    #:set FTYPE = FORTRAN_TYPES[TYPE]
    #:set HASLENGTH = HAS_LENGTH[TYPE]
    #:set MPITYPE = MPI_TYPES[TYPE]

    $:mpifx_allreduce_template(SUFFIX, FTYPE, MPITYPE, RANK)
    $:mpifx_allreduceip_template(SUFFIX, FTYPE, MPITYPE, RANK)

  #:endfor
#:endfor

end module mpifx_allreduce_module
