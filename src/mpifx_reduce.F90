include(mpifx_reduce.m4)

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
    module procedure mpifx_reduce_i0, mpifx_reduce_i1, mpifx_reduce_i2, &
        & mpifx_reduce_i3, mpifx_reduce_i4, mpifx_reduce_i5, mpifx_reduce_i6
    module procedure mpifx_reduce_s0, mpifx_reduce_s1, mpifx_reduce_s2, &
        & mpifx_reduce_s3, mpifx_reduce_s4, mpifx_reduce_s5, mpifx_reduce_s6
    module procedure mpifx_reduce_d0, mpifx_reduce_d1, mpifx_reduce_d2, &
        & mpifx_reduce_d3, mpifx_reduce_d4, mpifx_reduce_d5, mpifx_reduce_d6
    module procedure mpifx_reduce_c0, mpifx_reduce_c1, mpifx_reduce_c2, &
        & mpifx_reduce_c3, mpifx_reduce_c4, mpifx_reduce_c5, mpifx_reduce_c6
    module procedure mpifx_reduce_z0, mpifx_reduce_z1, mpifx_reduce_z2, &
        & mpifx_reduce_z3, mpifx_reduce_z4, mpifx_reduce_z5, mpifx_reduce_z6
    module procedure mpifx_reduce_l0, mpifx_reduce_l1, mpifx_reduce_l2, &
        & mpifx_reduce_l3, mpifx_reduce_l4, mpifx_reduce_l5, mpifx_reduce_l6
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
    module procedure mpifx_reduceip_i0, mpifx_reduceip_i1, mpifx_reduceip_i2, &
        & mpifx_reduceip_i3, mpifx_reduceip_i4, mpifx_reduceip_i5, &
        & mpifx_reduceip_i6
    module procedure mpifx_reduceip_s0, mpifx_reduceip_s1, mpifx_reduceip_s2, &
        & mpifx_reduceip_s3, mpifx_reduceip_s4, mpifx_reduceip_s5, &
        & mpifx_reduceip_s6
    module procedure mpifx_reduceip_d0, mpifx_reduceip_d1, mpifx_reduceip_d2, &
        & mpifx_reduceip_d3, mpifx_reduceip_d4, mpifx_reduceip_d5, &
        & mpifx_reduceip_d6
    module procedure mpifx_reduceip_c0, mpifx_reduceip_c1, mpifx_reduceip_c2, &
        & mpifx_reduceip_c3, mpifx_reduceip_c4, mpifx_reduceip_c5, &
        & mpifx_reduceip_c6
    module procedure mpifx_reduceip_z0, mpifx_reduceip_z1, mpifx_reduceip_z2, &
        & mpifx_reduceip_z3, mpifx_reduceip_z4, mpifx_reduceip_z5, &
        & mpifx_reduceip_z6
    module procedure mpifx_reduceip_l0, mpifx_reduceip_l1, mpifx_reduceip_l2, &
        & mpifx_reduceip_l3, mpifx_reduceip_l4, mpifx_reduceip_l5, &
        & mpifx_reduceip_l6
  end interface mpifx_reduceip


contains

  _subroutine_mpifx_reduce(i0, integer, , 1, MPI_INTEGER)
  _subroutine_mpifx_reduce(i1, integer, (:), size(operand), MPI_INTEGER)
  _subroutine_mpifx_reduce(i2, integer, (:,:), size(operand), MPI_INTEGER)
  _subroutine_mpifx_reduce(i3, integer, (:,:,:), size(operand), MPI_INTEGER)
  _subroutine_mpifx_reduce(i4, integer, (:,:,:,:), size(operand), MPI_INTEGER)
  _subroutine_mpifx_reduce(i5, integer, (:,:,:,:,:), size(operand), MPI_INTEGER)
  _subroutine_mpifx_reduce(i6, integer, (:,:,:,:,:,:), size(operand), 
      MPI_INTEGER)

  _subroutine_mpifx_reduce(s0, real(sp), , 1, MPI_REAL)
  _subroutine_mpifx_reduce(s1, real(sp), (:), size(operand), MPI_REAL)
  _subroutine_mpifx_reduce(s2, real(sp), (:,:), size(operand), MPI_REAL)
  _subroutine_mpifx_reduce(s3, real(sp), (:,:,:), size(operand), MPI_REAL)
  _subroutine_mpifx_reduce(s4, real(sp), (:,:,:,:), size(operand), MPI_REAL)
  _subroutine_mpifx_reduce(s5, real(sp), (:,:,:,:,:), size(operand), MPI_REAL)
  _subroutine_mpifx_reduce(s6, real(sp), (:,:,:,:,:,:), size(operand),
      MPI_REAL)
  
  _subroutine_mpifx_reduce(d0, real(dp), , 1, MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_reduce(d1, real(dp), (:), size(operand),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_reduce(d2, real(dp), (:,:), size(operand),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_reduce(d3, real(dp), (:,:,:), size(operand),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_reduce(d4, real(dp), (:,:,:,:), size(operand),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_reduce(d5, real(dp), (:,:,:,:,:), size(operand), 
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_reduce(d6, real(dp), (:,:,:,:,:,:), size(operand),
      MPI_DOUBLE_PRECISION)

  _subroutine_mpifx_reduce(c0, complex(sp), , 1, MPI_COMPLEX)
  _subroutine_mpifx_reduce(c1, complex(sp), (:), size(operand), MPI_COMPLEX)
  _subroutine_mpifx_reduce(c2, complex(sp), (:,:), size(operand), MPI_COMPLEX)
  _subroutine_mpifx_reduce(c3, complex(sp), (:,:,:), size(operand), MPI_COMPLEX)
  _subroutine_mpifx_reduce(c4, complex(sp), (:,:,:,:), size(operand),
      MPI_COMPLEX)
  _subroutine_mpifx_reduce(c5, complex(sp), (:,:,:,:,:), size(operand),
      MPI_COMPLEX)
  _subroutine_mpifx_reduce(c6, complex(sp), (:,:,:,:,:,:), size(operand),
      MPI_COMPLEX)

  _subroutine_mpifx_reduce(z0, complex(dp), , 1, MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_reduce(z1, complex(dp), (:), size(operand),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_reduce(z2, complex(dp), (:,:), size(operand),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_reduce(z3, complex(dp), (:,:,:), size(operand),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_reduce(z4, complex(dp), (:,:,:,:), size(operand),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_reduce(z5, complex(dp), (:,:,:,:,:), size(operand),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_reduce(z6, complex(dp), (:,:,:,:,:,:), size(operand), 
      MPI_DOUBLE_COMPLEX)

  _subroutine_mpifx_reduce(l0, logical, , 1, MPI_LOGICAL)
  _subroutine_mpifx_reduce(l1, logical, (:), size(operand), MPI_LOGICAL)
  _subroutine_mpifx_reduce(l2, logical, (:,:), size(operand), MPI_LOGICAL)
  _subroutine_mpifx_reduce(l3, logical, (:,:,:), size(operand), MPI_LOGICAL)
  _subroutine_mpifx_reduce(l4, logical, (:,:,:,:), size(operand), MPI_LOGICAL)
  _subroutine_mpifx_reduce(l5, logical, (:,:,:,:,:), size(operand), MPI_LOGICAL)
  _subroutine_mpifx_reduce(l6, logical, (:,:,:,:,:,:), size(operand),
      MPI_LOGICAL)

  _subroutine_mpifx_reduceip(i0, integer, , 1, MPI_INTEGER)
  _subroutine_mpifx_reduceip(i1, integer, (:), size(opres), MPI_INTEGER)
  _subroutine_mpifx_reduceip(i2, integer, (:,:), size(opres), MPI_INTEGER)
  _subroutine_mpifx_reduceip(i3, integer, (:,:,:), size(opres), MPI_INTEGER)
  _subroutine_mpifx_reduceip(i4, integer, (:,:,:,:), size(opres), MPI_INTEGER)
  _subroutine_mpifx_reduceip(i5, integer, (:,:,:,:,:), size(opres),
       MPI_INTEGER)
  _subroutine_mpifx_reduceip(i6, integer, (:,:,:,:,:,:), size(opres), 
      MPI_INTEGER)

  _subroutine_mpifx_reduceip(s0, real(sp), , 1, MPI_REAL)
  _subroutine_mpifx_reduceip(s1, real(sp), (:), size(opres), MPI_REAL)
  _subroutine_mpifx_reduceip(s2, real(sp), (:,:), size(opres), MPI_REAL)
  _subroutine_mpifx_reduceip(s3, real(sp), (:,:,:), size(opres), MPI_REAL)
  _subroutine_mpifx_reduceip(s4, real(sp), (:,:,:,:), size(opres), MPI_REAL)
  _subroutine_mpifx_reduceip(s5, real(sp), (:,:,:,:,:), size(opres), MPI_REAL)
  _subroutine_mpifx_reduceip(s6, real(sp), (:,:,:,:,:,:), size(opres),
      MPI_REAL)
  
  _subroutine_mpifx_reduceip(d0, real(dp), , 1, MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_reduceip(d1, real(dp), (:), size(opres),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_reduceip(d2, real(dp), (:,:), size(opres),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_reduceip(d3, real(dp), (:,:,:), size(opres),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_reduceip(d4, real(dp), (:,:,:,:), size(opres),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_reduceip(d5, real(dp), (:,:,:,:,:), size(opres), 
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_reduceip(d6, real(dp), (:,:,:,:,:,:), size(opres),
      MPI_DOUBLE_PRECISION)

  _subroutine_mpifx_reduceip(c0, complex(sp), , 1, MPI_COMPLEX)
  _subroutine_mpifx_reduceip(c1, complex(sp), (:), size(opres), MPI_COMPLEX)
  _subroutine_mpifx_reduceip(c2, complex(sp), (:,:), size(opres), MPI_COMPLEX)
  _subroutine_mpifx_reduceip(c3, complex(sp), (:,:,:), size(opres),
      MPI_COMPLEX)
  _subroutine_mpifx_reduceip(c4, complex(sp), (:,:,:,:), size(opres),
      MPI_COMPLEX)
  _subroutine_mpifx_reduceip(c5, complex(sp), (:,:,:,:,:), size(opres),
      MPI_COMPLEX)
  _subroutine_mpifx_reduceip(c6, complex(sp), (:,:,:,:,:,:), size(opres),
      MPI_COMPLEX)

  _subroutine_mpifx_reduceip(z0, complex(dp), , 1, MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_reduceip(z1, complex(dp), (:), size(opres),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_reduceip(z2, complex(dp), (:,:), size(opres),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_reduceip(z3, complex(dp), (:,:,:), size(opres),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_reduceip(z4, complex(dp), (:,:,:,:), size(opres),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_reduceip(z5, complex(dp), (:,:,:,:,:), size(opres),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_reduceip(z6, complex(dp), (:,:,:,:,:,:), size(opres), 
      MPI_DOUBLE_COMPLEX)

  _subroutine_mpifx_reduceip(l0, logical, , 1, MPI_LOGICAL)
  _subroutine_mpifx_reduceip(l1, logical, (:), size(opres), MPI_LOGICAL)
  _subroutine_mpifx_reduceip(l2, logical, (:,:), size(opres), MPI_LOGICAL)
  _subroutine_mpifx_reduceip(l3, logical, (:,:,:), size(opres), MPI_LOGICAL)
  _subroutine_mpifx_reduceip(l4, logical, (:,:,:,:), size(opres), MPI_LOGICAL)
  _subroutine_mpifx_reduceip(l5, logical, (:,:,:,:,:), size(opres),
      MPI_LOGICAL)
  _subroutine_mpifx_reduceip(l6, logical, (:,:,:,:,:,:), size(opres),
      MPI_LOGICAL)


end module mpifx_reduce_module
