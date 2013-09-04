include(mpifx_allreduce.m4)

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
    module procedure &
        & mpifx_allreduce_i0, mpifx_allreduce_i1, mpifx_allreduce_i2, &
        & mpifx_allreduce_i3, mpifx_allreduce_i4, mpifx_allreduce_i5, &
        & mpifx_allreduce_i6
    module procedure &
        & mpifx_allreduce_s0, mpifx_allreduce_s1, mpifx_allreduce_s2, &
        & mpifx_allreduce_s3, mpifx_allreduce_s4, mpifx_allreduce_s5, &
        & mpifx_allreduce_s6
    module procedure &
        & mpifx_allreduce_d0, mpifx_allreduce_d1, mpifx_allreduce_d2, &
        & mpifx_allreduce_d3, mpifx_allreduce_d4, mpifx_allreduce_d5, &
        & mpifx_allreduce_d6
    module procedure &
        & mpifx_allreduce_c0, mpifx_allreduce_c1, mpifx_allreduce_c2, &
        & mpifx_allreduce_c3, mpifx_allreduce_c4, mpifx_allreduce_c5, &
        & mpifx_allreduce_c6
    module procedure &
        & mpifx_allreduce_z0, mpifx_allreduce_z1, mpifx_allreduce_z2, &
        & mpifx_allreduce_z3, mpifx_allreduce_z4, mpifx_allreduce_z5, &
        & mpifx_allreduce_z6
    module procedure &
        & mpifx_allreduce_l0, mpifx_allreduce_l1, mpifx_allreduce_l2, &
        & mpifx_allreduce_l3, mpifx_allreduce_l4, mpifx_allreduce_l5, &
        & mpifx_allreduce_l6
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
    module procedure &
        & mpifx_allreduceip_i0, mpifx_allreduceip_i1, mpifx_allreduceip_i2, &
        & mpifx_allreduceip_i3, mpifx_allreduceip_i4, mpifx_allreduceip_i5, &
        & mpifx_allreduceip_i6
    module procedure &
        & mpifx_allreduceip_s0, mpifx_allreduceip_s1, mpifx_allreduceip_s2, &
        & mpifx_allreduceip_s3, mpifx_allreduceip_s4, mpifx_allreduceip_s5, &
        & mpifx_allreduceip_s6
    module procedure &
        & mpifx_allreduceip_d0, mpifx_allreduceip_d1, mpifx_allreduceip_d2, &
        & mpifx_allreduceip_d3, mpifx_allreduceip_d4, mpifx_allreduceip_d5, &
        & mpifx_allreduceip_d6
    module procedure &
        & mpifx_allreduceip_c0, mpifx_allreduceip_c1, mpifx_allreduceip_c2, &
        & mpifx_allreduceip_c3, mpifx_allreduceip_c4, mpifx_allreduceip_c5, &
        & mpifx_allreduceip_c6
    module procedure &
        & mpifx_allreduceip_z0, mpifx_allreduceip_z1, mpifx_allreduceip_z2, &
        & mpifx_allreduceip_z3, mpifx_allreduceip_z4, mpifx_allreduceip_z5, &
        & mpifx_allreduceip_z6
    module procedure &
        & mpifx_allreduceip_l0, mpifx_allreduceip_l1, mpifx_allreduceip_l2, &
        & mpifx_allreduceip_l3, mpifx_allreduceip_l4, mpifx_allreduceip_l5, &
        & mpifx_allreduceip_l6
  end interface mpifx_allreduceip

contains

  _subroutine_mpifx_allreduce(i0, integer, , 1, MPI_INTEGER)
  _subroutine_mpifx_allreduce(i1, integer, (:), size(operand), MPI_INTEGER)
  _subroutine_mpifx_allreduce(i2, integer, (:,:), size(operand), MPI_INTEGER)
  _subroutine_mpifx_allreduce(i3, integer, (:,:,:), size(operand), MPI_INTEGER)
  _subroutine_mpifx_allreduce(i4, integer, (:,:,:,:), size(operand), 
      MPI_INTEGER)
  _subroutine_mpifx_allreduce(i5, integer, (:,:,:,:,:), size(operand),
      MPI_INTEGER)
  _subroutine_mpifx_allreduce(i6, integer, (:,:,:,:,:,:), size(operand), 
      MPI_INTEGER)

  _subroutine_mpifx_allreduce(s0, real(sp), , 1, MPI_REAL)
  _subroutine_mpifx_allreduce(s1, real(sp), (:), size(operand), MPI_REAL)
  _subroutine_mpifx_allreduce(s2, real(sp), (:,:), size(operand), MPI_REAL)
  _subroutine_mpifx_allreduce(s3, real(sp), (:,:,:), size(operand), MPI_REAL)
  _subroutine_mpifx_allreduce(s4, real(sp), (:,:,:,:), size(operand), MPI_REAL)
  _subroutine_mpifx_allreduce(s5, real(sp), (:,:,:,:,:), size(operand), 
      MPI_REAL)
  _subroutine_mpifx_allreduce(s6, real(sp), (:,:,:,:,:,:), size(operand),
      MPI_REAL)
  
  _subroutine_mpifx_allreduce(d0, real(dp), , 1, MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allreduce(d1, real(dp), (:), size(operand),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allreduce(d2, real(dp), (:,:), size(operand),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allreduce(d3, real(dp), (:,:,:), size(operand),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allreduce(d4, real(dp), (:,:,:,:), size(operand),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allreduce(d5, real(dp), (:,:,:,:,:), size(operand), 
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allreduce(d6, real(dp), (:,:,:,:,:,:), size(operand),
      MPI_DOUBLE_PRECISION)

  _subroutine_mpifx_allreduce(c0, complex(sp), , 1, MPI_COMPLEX)
  _subroutine_mpifx_allreduce(c1, complex(sp), (:), size(operand), MPI_COMPLEX)
  _subroutine_mpifx_allreduce(c2, complex(sp), (:,:), size(operand), 
      MPI_COMPLEX)
  _subroutine_mpifx_allreduce(c3, complex(sp), (:,:,:), size(operand),
      MPI_COMPLEX)
  _subroutine_mpifx_allreduce(c4, complex(sp), (:,:,:,:), size(operand),
      MPI_COMPLEX)
  _subroutine_mpifx_allreduce(c5, complex(sp), (:,:,:,:,:), size(operand),
      MPI_COMPLEX)
  _subroutine_mpifx_allreduce(c6, complex(sp), (:,:,:,:,:,:), size(operand),
      MPI_COMPLEX)

  _subroutine_mpifx_allreduce(z0, complex(dp), , 1, MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allreduce(z1, complex(dp), (:), size(operand),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allreduce(z2, complex(dp), (:,:), size(operand),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allreduce(z3, complex(dp), (:,:,:), size(operand),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allreduce(z4, complex(dp), (:,:,:,:), size(operand),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allreduce(z5, complex(dp), (:,:,:,:,:), size(operand),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allreduce(z6, complex(dp), (:,:,:,:,:,:), size(operand), 
      MPI_DOUBLE_COMPLEX)

  _subroutine_mpifx_allreduce(l0, logical, , 1, MPI_LOGICAL)
  _subroutine_mpifx_allreduce(l1, logical, (:), size(operand), MPI_LOGICAL)
  _subroutine_mpifx_allreduce(l2, logical, (:,:), size(operand), MPI_LOGICAL)
  _subroutine_mpifx_allreduce(l3, logical, (:,:,:), size(operand), MPI_LOGICAL)
  _subroutine_mpifx_allreduce(l4, logical, (:,:,:,:), size(operand), 
      MPI_LOGICAL)
  _subroutine_mpifx_allreduce(l5, logical, (:,:,:,:,:), size(operand),
      MPI_LOGICAL)
  _subroutine_mpifx_allreduce(l6, logical, (:,:,:,:,:,:), size(operand),
      MPI_LOGICAL)

  _subroutine_mpifx_allreduceip(i0, integer, , 1, MPI_INTEGER)
  _subroutine_mpifx_allreduceip(i1, integer, (:), size(opres), MPI_INTEGER)
  _subroutine_mpifx_allreduceip(i2, integer, (:,:), size(opres), MPI_INTEGER)
  _subroutine_mpifx_allreduceip(i3, integer, (:,:,:), size(opres),
      MPI_INTEGER)
  _subroutine_mpifx_allreduceip(i4, integer, (:,:,:,:), size(opres), 
      MPI_INTEGER)
  _subroutine_mpifx_allreduceip(i5, integer, (:,:,:,:,:), size(opres),
      MPI_INTEGER)
  _subroutine_mpifx_allreduceip(i6, integer, (:,:,:,:,:,:), size(opres), 
      MPI_INTEGER)

  _subroutine_mpifx_allreduceip(s0, real(sp), , 1, MPI_REAL)
  _subroutine_mpifx_allreduceip(s1, real(sp), (:), size(opres), MPI_REAL)
  _subroutine_mpifx_allreduceip(s2, real(sp), (:,:), size(opres), MPI_REAL)
  _subroutine_mpifx_allreduceip(s3, real(sp), (:,:,:), size(opres), MPI_REAL)
  _subroutine_mpifx_allreduceip(s4, real(sp), (:,:,:,:), size(opres),
      MPI_REAL)
  _subroutine_mpifx_allreduceip(s5, real(sp), (:,:,:,:,:), size(opres), 
      MPI_REAL)
  _subroutine_mpifx_allreduceip(s6, real(sp), (:,:,:,:,:,:), size(opres),
      MPI_REAL)
  
  _subroutine_mpifx_allreduceip(d0, real(dp), , 1, MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allreduceip(d1, real(dp), (:), size(opres),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allreduceip(d2, real(dp), (:,:), size(opres),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allreduceip(d3, real(dp), (:,:,:), size(opres),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allreduceip(d4, real(dp), (:,:,:,:), size(opres),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allreduceip(d5, real(dp), (:,:,:,:,:), size(opres), 
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allreduceip(d6, real(dp), (:,:,:,:,:,:), size(opres),
      MPI_DOUBLE_PRECISION)

  _subroutine_mpifx_allreduceip(c0, complex(sp), , 1, MPI_COMPLEX)
  _subroutine_mpifx_allreduceip(c1, complex(sp), (:), size(opres),
      MPI_COMPLEX)
  _subroutine_mpifx_allreduceip(c2, complex(sp), (:,:), size(opres), 
      MPI_COMPLEX)
  _subroutine_mpifx_allreduceip(c3, complex(sp), (:,:,:), size(opres),
      MPI_COMPLEX)
  _subroutine_mpifx_allreduceip(c4, complex(sp), (:,:,:,:), size(opres),
      MPI_COMPLEX)
  _subroutine_mpifx_allreduceip(c5, complex(sp), (:,:,:,:,:), size(opres),
      MPI_COMPLEX)
  _subroutine_mpifx_allreduceip(c6, complex(sp), (:,:,:,:,:,:), size(opres),
      MPI_COMPLEX)

  _subroutine_mpifx_allreduceip(z0, complex(dp), , 1, MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allreduceip(z1, complex(dp), (:), size(opres),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allreduceip(z2, complex(dp), (:,:), size(opres),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allreduceip(z3, complex(dp), (:,:,:), size(opres),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allreduceip(z4, complex(dp), (:,:,:,:), size(opres),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allreduceip(z5, complex(dp), (:,:,:,:,:), size(opres),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allreduceip(z6, complex(dp), (:,:,:,:,:,:), size(opres), 
      MPI_DOUBLE_COMPLEX)

  _subroutine_mpifx_allreduceip(l0, logical, , 1, MPI_LOGICAL)
  _subroutine_mpifx_allreduceip(l1, logical, (:), size(opres), MPI_LOGICAL)
  _subroutine_mpifx_allreduceip(l2, logical, (:,:), size(opres), MPI_LOGICAL)
  _subroutine_mpifx_allreduceip(l3, logical, (:,:,:), size(opres), 
      MPI_LOGICAL)
  _subroutine_mpifx_allreduceip(l4, logical, (:,:,:,:), size(opres), 
      MPI_LOGICAL)
  _subroutine_mpifx_allreduceip(l5, logical, (:,:,:,:,:), size(opres),
      MPI_LOGICAL)
  _subroutine_mpifx_allreduceip(l6, logical, (:,:,:,:,:,:), size(opres),
      MPI_LOGICAL)


end module mpifx_allreduce_module
