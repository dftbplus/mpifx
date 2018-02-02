include(mpifx_gatherv.m4)

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
  !!       if (mycomm%master) then
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
  !!       if (mycomm%master) then
  !!         write(*, *) mycomm%rank, "Recv1 buffer:", recv1
  !!       end if
  !!     
  !!       call mpifx_finalize()
  !!       
  !!     end program test_gatherv
  !!
  interface mpifx_gatherv
    module procedure &
        & mpifx_gatherv_i1i1, mpifx_gatherv_i2i2, mpifx_gatherv_i3i3, &
        & mpifx_gatherv_i4i4, mpifx_gatherv_i5i5, mpifx_gatherv_i6i6
    module procedure &
        & mpifx_gatherv_s1s1, mpifx_gatherv_s2s2, mpifx_gatherv_s3s3, &
        & mpifx_gatherv_s4s4, mpifx_gatherv_s5s5, mpifx_gatherv_s6s6
    module procedure &
        & mpifx_gatherv_d1d1, mpifx_gatherv_d2d2, mpifx_gatherv_d3d3, &
        & mpifx_gatherv_d4d4, mpifx_gatherv_d5d5, mpifx_gatherv_d6d6
    module procedure &
        & mpifx_gatherv_c1c1, mpifx_gatherv_c2c2, mpifx_gatherv_c3c3, &
        & mpifx_gatherv_c4c4, mpifx_gatherv_c5c5, mpifx_gatherv_c6c6
    module procedure &
        & mpifx_gatherv_z1z1, mpifx_gatherv_z2z2, mpifx_gatherv_z3z3, &
        & mpifx_gatherv_z4z4, mpifx_gatherv_z5z5, mpifx_gatherv_z6z6
    module procedure &
        & mpifx_gatherv_l1l1, mpifx_gatherv_l2l2, mpifx_gatherv_l3l3, &
        & mpifx_gatherv_l4l4, mpifx_gatherv_l5l5, mpifx_gatherv_l6l6
  end interface mpifx_gatherv


contains

  _subroutine_mpifx_gatherv_dr0(i1i1, integer, (:), 1, MPI_INTEGER)
  _subroutine_mpifx_gatherv_dr0(i2i2, integer, (:,:), 2, MPI_INTEGER)
  _subroutine_mpifx_gatherv_dr0(i3i3, integer, (:,:,:), 3, MPI_INTEGER)
  _subroutine_mpifx_gatherv_dr0(i4i4, integer, (:,:,:,:), 4, MPI_INTEGER)
  _subroutine_mpifx_gatherv_dr0(i5i5, integer, (:,:,:,:,:), 5, MPI_INTEGER)
  _subroutine_mpifx_gatherv_dr0(i6i6, integer, (:,:,:,:,:,:), 6, MPI_INTEGER)


  _subroutine_mpifx_gatherv_dr0(s1s1, real(sp), (:), 1, MPI_REAL)
  _subroutine_mpifx_gatherv_dr0(s2s2, real(sp), (:,:), 2, MPI_REAL)
  _subroutine_mpifx_gatherv_dr0(s3s3, real(sp), (:,:,:), 3, MPI_REAL)
  _subroutine_mpifx_gatherv_dr0(s4s4, real(sp), (:,:,:,:), 4, MPI_REAL)
  _subroutine_mpifx_gatherv_dr0(s5s5, real(sp), (:,:,:,:,:), 5, MPI_REAL)
  _subroutine_mpifx_gatherv_dr0(s6s6, real(sp), (:,:,:,:,:,:), 6, MPI_REAL)


  _subroutine_mpifx_gatherv_dr0(d1d1, real(dp), (:), 1, MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_gatherv_dr0(d2d2, real(dp), (:,:), 2, MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_gatherv_dr0(d3d3, real(dp), (:,:,:), 3, MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_gatherv_dr0(d4d4, real(dp), (:,:,:,:), 4, 
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_gatherv_dr0(d5d5, real(dp), (:,:,:,:,:), 5, 
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_gatherv_dr0(d6d6, real(dp), (:,:,:,:,:,:), 6, 
      MPI_DOUBLE_PRECISION)


  _subroutine_mpifx_gatherv_dr0(c1c1, complex(sp), (:), 1, MPI_COMPLEX)
  _subroutine_mpifx_gatherv_dr0(c2c2, complex(sp), (:,:), 2, MPI_COMPLEX)
  _subroutine_mpifx_gatherv_dr0(c3c3, complex(sp), (:,:,:), 3, MPI_COMPLEX)
  _subroutine_mpifx_gatherv_dr0(c4c4, complex(sp), (:,:,:,:), 4, MPI_COMPLEX)
  _subroutine_mpifx_gatherv_dr0(c5c5, complex(sp), (:,:,:,:,:), 5, MPI_COMPLEX)
  _subroutine_mpifx_gatherv_dr0(c6c6, complex(sp), (:,:,:,:,:,:), 6, MPI_COMPLEX)


  _subroutine_mpifx_gatherv_dr0(z1z1, complex(dp), (:), 1, MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_gatherv_dr0(z2z2, complex(dp), (:,:), 2, MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_gatherv_dr0(z3z3, complex(dp), (:,:,:), 3,
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_gatherv_dr0(z4z4, complex(dp), (:,:,:,:), 4, 
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_gatherv_dr0(z5z5, complex(dp), (:,:,:,:,:), 5, 
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_gatherv_dr0(z6z6, complex(dp), (:,:,:,:,:,:), 6, 
      MPI_DOUBLE_COMPLEX)


  _subroutine_mpifx_gatherv_dr0(l1l1, logical, (:), 1, MPI_LOGICAL)
  _subroutine_mpifx_gatherv_dr0(l2l2, logical, (:,:), 2, MPI_LOGICAL)
  _subroutine_mpifx_gatherv_dr0(l3l3, logical, (:,:,:), 3, MPI_LOGICAL)
  _subroutine_mpifx_gatherv_dr0(l4l4, logical, (:,:,:,:), 4, MPI_LOGICAL)
  _subroutine_mpifx_gatherv_dr0(l5l5, logical, (:,:,:,:,:), 5, MPI_LOGICAL)
  _subroutine_mpifx_gatherv_dr0(l6l6, logical, (:,:,:,:,:,:), 6, MPI_LOGICAL)


end module mpifx_gatherv_module
