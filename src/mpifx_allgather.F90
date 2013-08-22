include(mpifx_allgather.m4)

!> Contains wrapper for \c MPI_ALLGATHER
module mpifx_allgather_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_allgather

  !> Gathers scalars/arrays on all nodes.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second and third arguments. The second and third
  !! arguments can be of type integer (i), real (s), double precision (d),
  !! complex (c), double complex (z) and logical (l). Their rank can vary from
  !! zero (scalars) up to the maximum rank. Both arguments must be of same
  !! type. The third argument must have the size of the second times the number
  !! of processes taking part in the gathering. The third argument must have
  !! either the same rank as the second one or one rank more. In that case
  !! the last dimension of it must be of the size of the number of processes
  !! in the gathering.
  !!
  !! \see MPI documentation (\c MPI_ALLGATHER)
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
    module procedure &
        & mpifx_allgather_i1i1, mpifx_allgather_i2i2, mpifx_allgather_i3i3, &
        & mpifx_allgather_i4i4, mpifx_allgather_i5i5, mpifx_allgather_i6i6
    module procedure &
        & mpifx_allgather_i0i1, mpifx_allgather_i1i2, mpifx_allgather_i2i3, &
        & mpifx_allgather_i3i4, mpifx_allgather_i4i5, mpifx_allgather_i5i6
    module procedure &
        & mpifx_allgather_s1s1, mpifx_allgather_s2s2, mpifx_allgather_s3s3, &
        & mpifx_allgather_s4s4, mpifx_allgather_s5s5, mpifx_allgather_s6s6
    module procedure &
        & mpifx_allgather_s0s1, mpifx_allgather_s1s2, mpifx_allgather_s2s3, &
        & mpifx_allgather_s3s4, mpifx_allgather_s4s5, mpifx_allgather_s5s6
    module procedure &
        & mpifx_allgather_d1d1, mpifx_allgather_d2d2, mpifx_allgather_d3d3, &
        & mpifx_allgather_d4d4, mpifx_allgather_d5d5, mpifx_allgather_d6d6
    module procedure &
        & mpifx_allgather_d0d1, mpifx_allgather_d1d2, mpifx_allgather_d2d3, &
        & mpifx_allgather_d3d4, mpifx_allgather_d4d5, mpifx_allgather_d5d6
    module procedure &
        & mpifx_allgather_c1c1, mpifx_allgather_c2c2, mpifx_allgather_c3c3, &
        & mpifx_allgather_c4c4, mpifx_allgather_c5c5, mpifx_allgather_c6c6
    module procedure &
        & mpifx_allgather_c0c1, mpifx_allgather_c1c2, mpifx_allgather_c2c3, &
        & mpifx_allgather_c3c4, mpifx_allgather_c4c5, mpifx_allgather_c5c6
    module procedure &
        & mpifx_allgather_z1z1, mpifx_allgather_z2z2, mpifx_allgather_z3z3, &
        & mpifx_allgather_z4z4, mpifx_allgather_z5z5, mpifx_allgather_z6z6
    module procedure &
        & mpifx_allgather_z0z1, mpifx_allgather_z1z2, mpifx_allgather_z2z3, &
        & mpifx_allgather_z3z4, mpifx_allgather_z4z5, mpifx_allgather_z5z6
    module procedure &
        & mpifx_allgather_l1l1, mpifx_allgather_l2l2, mpifx_allgather_l3l3, &
        & mpifx_allgather_l4l4, mpifx_allgather_l5l5, mpifx_allgather_l6l6
    module procedure &
        & mpifx_allgather_l0l1, mpifx_allgather_l1l2, mpifx_allgather_l2l3, &
        & mpifx_allgather_l3l4, mpifx_allgather_l4l5, mpifx_allgather_l5l6
  end interface mpifx_allgather


contains

  _subroutine_mpifx_allgather_dr0(i1i1, integer, (:), 1, MPI_INTEGER)
  _subroutine_mpifx_allgather_dr0(i2i2, integer, (:,:), 2, MPI_INTEGER)
  _subroutine_mpifx_allgather_dr0(i3i3, integer, (:,:,:), 3, MPI_INTEGER)
  _subroutine_mpifx_allgather_dr0(i4i4, integer, (:,:,:,:), 4, MPI_INTEGER)
  _subroutine_mpifx_allgather_dr0(i5i5, integer, (:,:,:,:,:), 5, MPI_INTEGER)
  _subroutine_mpifx_allgather_dr0(i6i6, integer, (:,:,:,:,:,:), 6, MPI_INTEGER)

  _subroutine_mpifx_allgather_dr1(i0i1, integer, , 1, (:), 1, MPI_INTEGER)
  _subroutine_mpifx_allgather_dr1(i1i2, integer, (:), size(send), (:,:), 2, 
      MPI_INTEGER)
  _subroutine_mpifx_allgather_dr1(i2i3, integer, (:,:), size(send), (:,:,:), 3, 
      MPI_INTEGER)
  _subroutine_mpifx_allgather_dr1(i3i4, integer, (:,:,:), size(send), (:,:,:,:),
      4, MPI_INTEGER)
  _subroutine_mpifx_allgather_dr1(i4i5, integer, (:,:,:,:), size(send), 
      (:,:,:,:,:), 5, MPI_INTEGER)
  _subroutine_mpifx_allgather_dr1(i5i6, integer, (:,:,:,:,:), size(send), 
      (:,:,:,:,:,:), 6, MPI_INTEGER)


  _subroutine_mpifx_allgather_dr0(s1s1, real(sp), (:), 1, MPI_REAL)
  _subroutine_mpifx_allgather_dr0(s2s2, real(sp), (:,:), 2, MPI_REAL)
  _subroutine_mpifx_allgather_dr0(s3s3, real(sp), (:,:,:), 3, MPI_REAL)
  _subroutine_mpifx_allgather_dr0(s4s4, real(sp), (:,:,:,:), 4, MPI_REAL)
  _subroutine_mpifx_allgather_dr0(s5s5, real(sp), (:,:,:,:,:), 5, MPI_REAL)
  _subroutine_mpifx_allgather_dr0(s6s6, real(sp), (:,:,:,:,:,:), 6, MPI_REAL)

  _subroutine_mpifx_allgather_dr1(s0s1, real(sp), , 1, (:), 1, MPI_REAL)
  _subroutine_mpifx_allgather_dr1(s1s2, real(sp), (:), size(send), (:,:), 2, 
      MPI_REAL)
  _subroutine_mpifx_allgather_dr1(s2s3, real(sp), (:,:), size(send), (:,:,:), 
      3, MPI_REAL)
  _subroutine_mpifx_allgather_dr1(s3s4, real(sp), (:,:,:), size(send), 
      (:,:,:,:), 4, MPI_REAL)
  _subroutine_mpifx_allgather_dr1(s4s5, real(sp), (:,:,:,:), size(send), 
      (:,:,:,:,:), 5, MPI_REAL)
  _subroutine_mpifx_allgather_dr1(s5s6, real(sp), (:,:,:,:,:), size(send), 
      (:,:,:,:,:,:), 6, MPI_REAL)


  _subroutine_mpifx_allgather_dr0(d1d1, real(dp), (:), 1, MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allgather_dr0(d2d2, real(dp), (:,:), 2,
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allgather_dr0(d3d3, real(dp), (:,:,:), 3,
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allgather_dr0(d4d4, real(dp), (:,:,:,:), 4, 
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allgather_dr0(d5d5, real(dp), (:,:,:,:,:), 5, 
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allgather_dr0(d6d6, real(dp), (:,:,:,:,:,:), 6, 
      MPI_DOUBLE_PRECISION)

  _subroutine_mpifx_allgather_dr1(d0d1, real(dp), , 1, (:), 1,
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allgather_dr1(d1d2, real(dp), (:), size(send), (:,:), 2, 
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allgather_dr1(d2d3, real(dp), (:,:), size(send), (:,:,:), 
      3, MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allgather_dr1(d3d4, real(dp), (:,:,:), size(send),
      (:,:,:,:), 4, MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allgather_dr1(d4d5, real(dp), (:,:,:,:), size(send), 
      (:,:,:,:,:), 5, MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_allgather_dr1(d5d6, real(dp), (:,:,:,:,:), size(send), 
      (:,:,:,:,:,:), 6, MPI_DOUBLE_PRECISION)


  _subroutine_mpifx_allgather_dr0(c1c1, complex(sp), (:), 1, MPI_COMPLEX)
  _subroutine_mpifx_allgather_dr0(c2c2, complex(sp), (:,:), 2, MPI_COMPLEX)
  _subroutine_mpifx_allgather_dr0(c3c3, complex(sp), (:,:,:), 3, MPI_COMPLEX)
  _subroutine_mpifx_allgather_dr0(c4c4, complex(sp), (:,:,:,:), 4, MPI_COMPLEX)
  _subroutine_mpifx_allgather_dr0(c5c5, complex(sp), (:,:,:,:,:), 5,
      MPI_COMPLEX)
  _subroutine_mpifx_allgather_dr0(c6c6, complex(sp), (:,:,:,:,:,:), 6,
      MPI_COMPLEX)

  _subroutine_mpifx_allgather_dr1(c0c1, complex(sp), , 1, (:), 1, MPI_COMPLEX)
  _subroutine_mpifx_allgather_dr1(c1c2, complex(sp), (:), size(send), (:,:), 2, 
      MPI_COMPLEX)
  _subroutine_mpifx_allgather_dr1(c2c3, complex(sp), (:,:), size(send),
      (:,:,:), 3, MPI_COMPLEX)
  _subroutine_mpifx_allgather_dr1(c3c4, complex(sp), (:,:,:), size(send),
      (:,:,:,:), 4, MPI_COMPLEX)
  _subroutine_mpifx_allgather_dr1(c4c5, complex(sp), (:,:,:,:), size(send), 
      (:,:,:,:,:), 5, MPI_COMPLEX)
  _subroutine_mpifx_allgather_dr1(c5c6, complex(sp), (:,:,:,:,:), size(send), 
      (:,:,:,:,:,:), 6, MPI_COMPLEX)


  _subroutine_mpifx_allgather_dr0(z1z1, complex(dp), (:), 1, MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allgather_dr0(z2z2, complex(dp), (:,:), 2,
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allgather_dr0(z3z3, complex(dp), (:,:,:), 3,
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allgather_dr0(z4z4, complex(dp), (:,:,:,:), 4, 
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allgather_dr0(z5z5, complex(dp), (:,:,:,:,:), 5, 
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allgather_dr0(z6z6, complex(dp), (:,:,:,:,:,:), 6, 
      MPI_DOUBLE_COMPLEX)

  _subroutine_mpifx_allgather_dr1(z0z1, complex(dp), , 1, (:), 1,
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allgather_dr1(z1z2, complex(dp), (:), size(send), (:,:), 2, 
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allgather_dr1(z2z3, complex(dp), (:,:), size(send), (:,:,:),
      3, MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allgather_dr1(z3z4, complex(dp), (:,:,:), size(send),
      (:,:,:,:), 4, MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allgather_dr1(z4z5, complex(dp), (:,:,:,:), size(send), 
      (:,:,:,:,:), 5, MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_allgather_dr1(z5z6, complex(dp), (:,:,:,:,:), size(send), 
      (:,:,:,:,:,:), 6, MPI_DOUBLE_COMPLEX)


  _subroutine_mpifx_allgather_dr0(l1l1, logical, (:), 1, MPI_LOGICAL)
  _subroutine_mpifx_allgather_dr0(l2l2, logical, (:,:), 2, MPI_LOGICAL)
  _subroutine_mpifx_allgather_dr0(l3l3, logical, (:,:,:), 3, MPI_LOGICAL)
  _subroutine_mpifx_allgather_dr0(l4l4, logical, (:,:,:,:), 4, MPI_LOGICAL)
  _subroutine_mpifx_allgather_dr0(l5l5, logical, (:,:,:,:,:), 5, MPI_LOGICAL)
  _subroutine_mpifx_allgather_dr0(l6l6, logical, (:,:,:,:,:,:), 6, MPI_LOGICAL)

  _subroutine_mpifx_allgather_dr1(l0l1, logical, , 1, (:), 1, MPI_LOGICAL)
  _subroutine_mpifx_allgather_dr1(l1l2, logical, (:), size(send), (:,:), 2, 
      MPI_LOGICAL)
  _subroutine_mpifx_allgather_dr1(l2l3, logical, (:,:), size(send), (:,:,:), 3, 
      MPI_LOGICAL)
  _subroutine_mpifx_allgather_dr1(l3l4, logical, (:,:,:), size(send), (:,:,:,:),
      4, MPI_LOGICAL)
  _subroutine_mpifx_allgather_dr1(l4l5, logical, (:,:,:,:), size(send), 
      (:,:,:,:,:), 5, MPI_LOGICAL)
  _subroutine_mpifx_allgather_dr1(l5l6, logical, (:,:,:,:,:), size(send), 
      (:,:,:,:,:,:), 6, MPI_LOGICAL)


end module mpifx_allgather_module
