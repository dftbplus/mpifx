include(mpifx_bcast.m4)

!> Contains wrapper for \c MPI_BCAST.
module mpifx_bcast_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_bcast

  !> Broadcasts an MPI message to all nodes.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second argument. The second argument can be of
  !! type integer (i), real (s), double precision (d), complex (c),
  !! double complex (z), logical (l) and character (h). Its rank can vary from
  !! zero (scalar) up to the maximum rank.
  !!
  !! \see MPI documentation (\c MPI_BCAST)
  !!
  !! Example:
  !!
  !!     program test_bcast
  !!       use libmpifx_module
  !!     
  !!       type(mpifx) :: mycomm
  !!       integer :: buffer(3)
  !!     
  !!       call mycomm%init()
  !!       if (mycomm%master) then
  !!         buffer(:) = [ 1, 2, 3 ]
  !!       end if
  !!       call mpifx_bcast(mycomm, buffer)
  !!       print "(A,I2.2,A,3I5)", "BUFFER:", mycomm%rank, ":", buffer
  !!       call mycomm%destruct()
  !!       
  !!     end program test_bcast
  !!
  interface mpifx_bcast
    module procedure mpifx_bcast_i0, mpifx_bcast_i1, mpifx_bcast_i2, &
        & mpifx_bcast_i3, mpifx_bcast_i4, mpifx_bcast_i5, mpifx_bcast_i6
    module procedure mpifx_bcast_s0, mpifx_bcast_s1, mpifx_bcast_s2, &
        & mpifx_bcast_s3, mpifx_bcast_s4, mpifx_bcast_s5, mpifx_bcast_s6
    module procedure mpifx_bcast_d0, mpifx_bcast_d1, mpifx_bcast_d2, &
        & mpifx_bcast_d3, mpifx_bcast_d4, mpifx_bcast_d5, mpifx_bcast_d6
    module procedure mpifx_bcast_c0, mpifx_bcast_c1, mpifx_bcast_c2, &
        & mpifx_bcast_c3, mpifx_bcast_c4, mpifx_bcast_c5, mpifx_bcast_c6
    module procedure mpifx_bcast_z0, mpifx_bcast_z1, mpifx_bcast_z2, &
        & mpifx_bcast_z3, mpifx_bcast_z4, mpifx_bcast_z5, mpifx_bcast_z6
    module procedure mpifx_bcast_l0, mpifx_bcast_l1, mpifx_bcast_l2, &
        & mpifx_bcast_l3, mpifx_bcast_l4, mpifx_bcast_l5, mpifx_bcast_l6
    module procedure mpifx_bcast_h0, mpifx_bcast_h1, mpifx_bcast_h2, &
        & mpifx_bcast_h3, mpifx_bcast_h4, mpifx_bcast_h5, mpifx_bcast_h6
  end interface

contains

  _subroutine_mpifx_bcast(i0, integer, , 1, MPI_INTEGER)
  _subroutine_mpifx_bcast(i1, integer, (:), size(msg), MPI_INTEGER)
  _subroutine_mpifx_bcast(i2, integer, (:,:), size(msg), MPI_INTEGER)
  _subroutine_mpifx_bcast(i3, integer, (:,:,:), size(msg), MPI_INTEGER)
  _subroutine_mpifx_bcast(i4, integer, (:,:,:,:), size(msg), MPI_INTEGER)
  _subroutine_mpifx_bcast(i5, integer, (:,:,:,:,:), size(msg), MPI_INTEGER)
  _subroutine_mpifx_bcast(i6, integer, (:,:,:,:,:,:), size(msg), MPI_INTEGER)

  _subroutine_mpifx_bcast(s0, real(sp), , 1, MPI_REAL)
  _subroutine_mpifx_bcast(s1, real(sp), (:), size(msg), MPI_REAL)
  _subroutine_mpifx_bcast(s2, real(sp), (:,:), size(msg), MPI_REAL)
  _subroutine_mpifx_bcast(s3, real(sp), (:,:,:), size(msg), MPI_REAL)
  _subroutine_mpifx_bcast(s4, real(sp), (:,:,:,:), size(msg), MPI_REAL)
  _subroutine_mpifx_bcast(s5, real(sp), (:,:,:,:,:), size(msg), MPI_REAL)
  _subroutine_mpifx_bcast(s6, real(sp), (:,:,:,:,:,:), size(msg), MPI_REAL)
  
  _subroutine_mpifx_bcast(d0, real(dp), , 1, MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_bcast(d1, real(dp), (:), size(msg), MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_bcast(d2, real(dp), (:,:), size(msg),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_bcast(d3, real(dp), (:,:,:), size(msg),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_bcast(d4, real(dp), (:,:,:,:), size(msg),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_bcast(d5, real(dp), (:,:,:,:,:), size(msg), 
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_bcast(d6, real(dp), (:,:,:,:,:,:), size(msg),
      MPI_DOUBLE_PRECISION)

  _subroutine_mpifx_bcast(c0, complex(sp), , 1, MPI_COMPLEX)
  _subroutine_mpifx_bcast(c1, complex(sp), (:), size(msg), MPI_COMPLEX)
  _subroutine_mpifx_bcast(c2, complex(sp), (:,:), size(msg), MPI_COMPLEX)
  _subroutine_mpifx_bcast(c3, complex(sp), (:,:,:), size(msg), MPI_COMPLEX)
  _subroutine_mpifx_bcast(c4, complex(sp), (:,:,:,:), size(msg), MPI_COMPLEX)
  _subroutine_mpifx_bcast(c5, complex(sp), (:,:,:,:,:), size(msg),
      MPI_COMPLEX)
  _subroutine_mpifx_bcast(c6, complex(sp), (:,:,:,:,:,:), size(msg),
      MPI_COMPLEX)

  _subroutine_mpifx_bcast(z0, complex(dp), , 1, MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_bcast(z1, complex(dp), (:), size(msg),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_bcast(z2, complex(dp), (:,:), size(msg),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_bcast(z3, complex(dp), (:,:,:), size(msg),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_bcast(z4, complex(dp), (:,:,:,:), size(msg),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_bcast(z5, complex(dp), (:,:,:,:,:), size(msg),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_bcast(z6, complex(dp), (:,:,:,:,:,:), size(msg), 
      MPI_DOUBLE_COMPLEX)

  _subroutine_mpifx_bcast(l0, logical, , 1, MPI_LOGICAL)
  _subroutine_mpifx_bcast(l1, logical, (:), size(msg), MPI_LOGICAL)
  _subroutine_mpifx_bcast(l2, logical, (:,:), size(msg), MPI_LOGICAL)
  _subroutine_mpifx_bcast(l3, logical, (:,:,:), size(msg), MPI_LOGICAL)
  _subroutine_mpifx_bcast(l4, logical, (:,:,:,:), size(msg), MPI_LOGICAL)
  _subroutine_mpifx_bcast(l5, logical, (:,:,:,:,:), size(msg), MPI_LOGICAL)
  _subroutine_mpifx_bcast(l6, logical, (:,:,:,:,:,:), size(msg), MPI_LOGICAL)

  _subroutine_mpifx_bcast(h0, character(*), , len(msg), MPI_CHARACTER)
  _subroutine_mpifx_bcast(h1, character(*), (:), size(msg) * len(msg),
      MPI_CHARACTER)
  _subroutine_mpifx_bcast(h2, character(*), (:,:),
      size(msg) * len(msg), MPI_CHARACTER)
  _subroutine_mpifx_bcast(h3, character(*), (:,:,:),
      size(msg) * len(msg), MPI_CHARACTER)
  _subroutine_mpifx_bcast(h4, character(*), (:,:,:,:),
      size(msg) * len(msg), MPI_CHARACTER)
  _subroutine_mpifx_bcast(h5, character(*), (:,:,:,:,:),
      size(msg) * len(msg), MPI_CHARACTER)
  _subroutine_mpifx_bcast(h6, character(*), (:,:,:,:,:,:),
      size(msg) * len(msg), MPI_CHARACTER)

end module mpifx_bcast_module
