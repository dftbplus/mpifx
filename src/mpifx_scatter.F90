include(mpifx_scatter.m4)

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
  !!       if (mycomm%master) then
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
  !!       if (mycomm%master) then
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
  !!       if (mycomm%master) then
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
    module procedure &
        & mpifx_scatter_i1i1, mpifx_scatter_i2i2, mpifx_scatter_i3i3, &
        & mpifx_scatter_i4i4, mpifx_scatter_i5i5, mpifx_scatter_i6i6
    module procedure &
        & mpifx_scatter_i1i0, mpifx_scatter_i2i1, mpifx_scatter_i3i2, &
        & mpifx_scatter_i4i3, mpifx_scatter_i5i4, mpifx_scatter_i6i5
    module procedure &
        & mpifx_scatter_s1s1, mpifx_scatter_s2s2, mpifx_scatter_s3s3, &
        & mpifx_scatter_s4s4, mpifx_scatter_s5s5, mpifx_scatter_s6s6
    module procedure &
        & mpifx_scatter_s1s0, mpifx_scatter_s2s1, mpifx_scatter_s3s2, &
        & mpifx_scatter_s4s3, mpifx_scatter_s5s4, mpifx_scatter_s6s5
    module procedure &
        & mpifx_scatter_d1d1, mpifx_scatter_d2d2, mpifx_scatter_d3d3, &
        & mpifx_scatter_d4d4, mpifx_scatter_d5d5, mpifx_scatter_d6d6
    module procedure &
        & mpifx_scatter_d1d0, mpifx_scatter_d2d1, mpifx_scatter_d3d2, &
        & mpifx_scatter_d4d3, mpifx_scatter_d5d4, mpifx_scatter_d6d5
    module procedure &
        & mpifx_scatter_c1c1, mpifx_scatter_c2c2, mpifx_scatter_c3c3, &
        & mpifx_scatter_c4c4, mpifx_scatter_c5c5, mpifx_scatter_c6c6
    module procedure &
        & mpifx_scatter_c1c0, mpifx_scatter_c2c1, mpifx_scatter_c3c2, &
        & mpifx_scatter_c4c3, mpifx_scatter_c5c4, mpifx_scatter_c6c5
    module procedure &
        & mpifx_scatter_z1z1, mpifx_scatter_z2z2, mpifx_scatter_z3z3, &
        & mpifx_scatter_z4z4, mpifx_scatter_z5z5, mpifx_scatter_z6z6
    module procedure &
        & mpifx_scatter_z1z0, mpifx_scatter_z2z1, mpifx_scatter_z3z2, &
        & mpifx_scatter_z4z3, mpifx_scatter_z5z4, mpifx_scatter_z6z5
    module procedure &
        & mpifx_scatter_l1l1, mpifx_scatter_l2l2, mpifx_scatter_l3l3, &
        & mpifx_scatter_l4l4, mpifx_scatter_l5l5, mpifx_scatter_l6l6
    module procedure &
        & mpifx_scatter_l1l0, mpifx_scatter_l2l1, mpifx_scatter_l3l2, &
        & mpifx_scatter_l4l3, mpifx_scatter_l5l4, mpifx_scatter_l6l5
  end interface mpifx_scatter


contains

  _subroutine_mpifx_scatter_dr0(i1i1, integer, (:), 1, MPI_INTEGER)
  _subroutine_mpifx_scatter_dr0(i2i2, integer, (:,:), 2, MPI_INTEGER)
  _subroutine_mpifx_scatter_dr0(i3i3, integer, (:,:,:), 3, MPI_INTEGER)
  _subroutine_mpifx_scatter_dr0(i4i4, integer, (:,:,:,:), 4, MPI_INTEGER)
  _subroutine_mpifx_scatter_dr0(i5i5, integer, (:,:,:,:,:), 5, MPI_INTEGER)
  _subroutine_mpifx_scatter_dr0(i6i6, integer, (:,:,:,:,:,:), 6, MPI_INTEGER)

  _subroutine_mpifx_scatter_dr1(i1i0, integer, , 1, (:), 1, MPI_INTEGER)
  _subroutine_mpifx_scatter_dr1(i2i1, integer, (:), size(recv), (:,:), 2, 
      MPI_INTEGER)
  _subroutine_mpifx_scatter_dr1(i3i2, integer, (:,:), size(recv), (:,:,:), 3, 
      MPI_INTEGER)
  _subroutine_mpifx_scatter_dr1(i4i3, integer, (:,:,:), size(recv), (:,:,:,:),
      4, MPI_INTEGER)
  _subroutine_mpifx_scatter_dr1(i5i4, integer, (:,:,:,:), size(recv), 
      (:,:,:,:,:), 5, MPI_INTEGER)
  _subroutine_mpifx_scatter_dr1(i6i5, integer, (:,:,:,:,:), size(recv), 
      (:,:,:,:,:,:), 6, MPI_INTEGER)


  _subroutine_mpifx_scatter_dr0(s1s1, real(sp), (:), 1, MPI_REAL)
  _subroutine_mpifx_scatter_dr0(s2s2, real(sp), (:,:), 2, MPI_REAL)
  _subroutine_mpifx_scatter_dr0(s3s3, real(sp), (:,:,:), 3, MPI_REAL)
  _subroutine_mpifx_scatter_dr0(s4s4, real(sp), (:,:,:,:), 4, MPI_REAL)
  _subroutine_mpifx_scatter_dr0(s5s5, real(sp), (:,:,:,:,:), 5, MPI_REAL)
  _subroutine_mpifx_scatter_dr0(s6s6, real(sp), (:,:,:,:,:,:), 6, MPI_REAL)

  _subroutine_mpifx_scatter_dr1(s1s0, real(sp), , 1, (:), 1, MPI_REAL)
  _subroutine_mpifx_scatter_dr1(s2s1, real(sp), (:), size(recv), (:,:), 2, 
      MPI_REAL)
  _subroutine_mpifx_scatter_dr1(s3s2, real(sp), (:,:), size(recv), (:,:,:), 3, 
      MPI_REAL)
  _subroutine_mpifx_scatter_dr1(s4s3, real(sp), (:,:,:), size(recv), (:,:,:,:),
      4, MPI_REAL)
  _subroutine_mpifx_scatter_dr1(s5s4, real(sp), (:,:,:,:), size(recv), 
      (:,:,:,:,:), 5, MPI_REAL)
  _subroutine_mpifx_scatter_dr1(s6s5, real(sp), (:,:,:,:,:), size(recv), 
      (:,:,:,:,:,:), 6, MPI_REAL)


  _subroutine_mpifx_scatter_dr0(d1d1, real(dp), (:), 1, MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_scatter_dr0(d2d2, real(dp), (:,:), 2, MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_scatter_dr0(d3d3, real(dp), (:,:,:), 3,
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_scatter_dr0(d4d4, real(dp), (:,:,:,:), 4, 
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_scatter_dr0(d5d5, real(dp), (:,:,:,:,:), 5, 
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_scatter_dr0(d6d6, real(dp), (:,:,:,:,:,:), 6, 
      MPI_DOUBLE_PRECISION)

  _subroutine_mpifx_scatter_dr1(d1d0, real(dp), , 1, (:), 1,
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_scatter_dr1(d2d1, real(dp), (:), size(recv), (:,:), 2, 
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_scatter_dr1(d3d2, real(dp), (:,:), size(recv), (:,:,:), 3, 
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_scatter_dr1(d4d3, real(dp), (:,:,:), size(recv), (:,:,:,:),
      4, MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_scatter_dr1(d5d4, real(dp), (:,:,:,:), size(recv), 
      (:,:,:,:,:), 5, MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_scatter_dr1(d6d5, real(dp), (:,:,:,:,:), size(recv), 
      (:,:,:,:,:,:), 6, MPI_DOUBLE_PRECISION)


  _subroutine_mpifx_scatter_dr0(c1c1, complex(sp), (:), 1, MPI_COMPLEX)
  _subroutine_mpifx_scatter_dr0(c2c2, complex(sp), (:,:), 2, MPI_COMPLEX)
  _subroutine_mpifx_scatter_dr0(c3c3, complex(sp), (:,:,:), 3, MPI_COMPLEX)
  _subroutine_mpifx_scatter_dr0(c4c4, complex(sp), (:,:,:,:), 4, MPI_COMPLEX)
  _subroutine_mpifx_scatter_dr0(c5c5, complex(sp), (:,:,:,:,:), 5, MPI_COMPLEX)
  _subroutine_mpifx_scatter_dr0(c6c6, complex(sp), (:,:,:,:,:,:), 6,
      MPI_COMPLEX)

  _subroutine_mpifx_scatter_dr1(c1c0, complex(sp), , 1, (:), 1, MPI_COMPLEX)
  _subroutine_mpifx_scatter_dr1(c2c1, complex(sp), (:), size(recv), (:,:), 2, 
      MPI_COMPLEX)
  _subroutine_mpifx_scatter_dr1(c3c2, complex(sp), (:,:), size(recv), (:,:,:),
      3, MPI_COMPLEX)
  _subroutine_mpifx_scatter_dr1(c4c3, complex(sp), (:,:,:), size(recv),
      (:,:,:,:), 4, MPI_COMPLEX)
  _subroutine_mpifx_scatter_dr1(c5c4, complex(sp), (:,:,:,:), size(recv), 
      (:,:,:,:,:), 5, MPI_COMPLEX)
  _subroutine_mpifx_scatter_dr1(c6c5, complex(sp), (:,:,:,:,:), size(recv), 
      (:,:,:,:,:,:), 6, MPI_COMPLEX)


  _subroutine_mpifx_scatter_dr0(z1z1, complex(dp), (:), 1, MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_scatter_dr0(z2z2, complex(dp), (:,:), 2, MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_scatter_dr0(z3z3, complex(dp), (:,:,:), 3,
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_scatter_dr0(z4z4, complex(dp), (:,:,:,:), 4, 
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_scatter_dr0(z5z5, complex(dp), (:,:,:,:,:), 5, 
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_scatter_dr0(z6z6, complex(dp), (:,:,:,:,:,:), 6, 
      MPI_DOUBLE_COMPLEX)

  _subroutine_mpifx_scatter_dr1(z1z0, complex(dp), , 1, (:), 1,
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_scatter_dr1(z2z1, complex(dp), (:), size(recv), (:,:), 2, 
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_scatter_dr1(z3z2, complex(dp), (:,:), size(recv), (:,:,:),
      3, MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_scatter_dr1(z4z3, complex(dp), (:,:,:), size(recv),
      (:,:,:,:), 4, MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_scatter_dr1(z5z4, complex(dp), (:,:,:,:), size(recv), 
      (:,:,:,:,:), 5, MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_scatter_dr1(z6z5, complex(dp), (:,:,:,:,:), size(recv), 
      (:,:,:,:,:,:), 6, MPI_DOUBLE_COMPLEX)


  _subroutine_mpifx_scatter_dr0(l1l1, logical, (:), 1, MPI_LOGICAL)
  _subroutine_mpifx_scatter_dr0(l2l2, logical, (:,:), 2, MPI_LOGICAL)
  _subroutine_mpifx_scatter_dr0(l3l3, logical, (:,:,:), 3, MPI_LOGICAL)
  _subroutine_mpifx_scatter_dr0(l4l4, logical, (:,:,:,:), 4, MPI_LOGICAL)
  _subroutine_mpifx_scatter_dr0(l5l5, logical, (:,:,:,:,:), 5, MPI_LOGICAL)
  _subroutine_mpifx_scatter_dr0(l6l6, logical, (:,:,:,:,:,:), 6, MPI_LOGICAL)

  _subroutine_mpifx_scatter_dr1(l1l0, logical, , 1, (:), 1, MPI_LOGICAL)
  _subroutine_mpifx_scatter_dr1(l2l1, logical, (:), size(recv), (:,:), 2, 
      MPI_LOGICAL)
  _subroutine_mpifx_scatter_dr1(l3l2, logical, (:,:), size(recv), (:,:,:), 3, 
      MPI_LOGICAL)
  _subroutine_mpifx_scatter_dr1(l4l3, logical, (:,:,:), size(recv), (:,:,:,:),
      4, MPI_LOGICAL)
  _subroutine_mpifx_scatter_dr1(l5l4, logical, (:,:,:,:), size(recv), 
      (:,:,:,:,:), 5, MPI_LOGICAL)
  _subroutine_mpifx_scatter_dr1(l6l5, logical, (:,:,:,:,:), size(recv), 
      (:,:,:,:,:,:), 6, MPI_LOGICAL)


end module mpifx_scatter_module
