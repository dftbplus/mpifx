include(mpifx_recv.m4)

!> Contains wrapper for \c MPI_RECV
module mpifx_recv_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_recv


  !> Receives a message from a given node.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second argument. The second argument can be of
  !! type integer (i), real (s), double precision (d), complex (c), 
  !! double complex (z), logical (b) and character (h). Its rank can vary from
  !! zero (scalar) up to the maximum rank.
  !!
  !! \see MPI documentation (\c MPI_RECV)
  !!
  !! Example:
  !!
  !!     program hello
  !!     use libmpifx_module
  !!     implicit none
  !!
  !!     character(100) :: msg
  !!     type(mpifx) :: mycomm
  !!     integer :: source
  !!
  !!     call mpifx_init()
  !!     call mycomm%init()
  !!     if (.not. mycomm%master) then
  !!       write(msg, "(A,I0,A)") "Hello from process ", mycomm%iproc, "!"
  !!       call mpifx_send(mycomm, msg, mycomm%imaster)
  !!     else
  !!       write(*, "(A)") "Master node:"
  !!       do source = 1, mycomm%nproc - 1
  !!         call mpifx_recv(mycomm, msg, source)
  !!         write(*,"(A,A)") "Message received: ", trim(msg)
  !!       end do
  !!     end if
  !!     call mpifx_finalize()
  !!
  !!   end program hello
  !!
  interface mpifx_recv
    module procedure mpifx_recv_i0, mpifx_recv_i1, mpifx_recv_i2, &
        & mpifx_recv_i3, mpifx_recv_i4, mpifx_recv_i5, mpifx_recv_i6
    module procedure mpifx_recv_l0, mpifx_recv_l1, mpifx_recv_l2, &
        & mpifx_recv_l3, mpifx_recv_l4, mpifx_recv_l5, mpifx_recv_l6
    module procedure mpifx_recv_s0, mpifx_recv_s1, mpifx_recv_s2, &
        & mpifx_recv_s3, mpifx_recv_s4, mpifx_recv_s5, mpifx_recv_s6
    module procedure mpifx_recv_d0, mpifx_recv_d1, mpifx_recv_d2, &
        & mpifx_recv_d3, mpifx_recv_d4, mpifx_recv_d5, mpifx_recv_d6
    module procedure mpifx_recv_c0, mpifx_recv_c1, mpifx_recv_c2, &
        & mpifx_recv_c3, mpifx_recv_c4, mpifx_recv_c5, mpifx_recv_c6
    module procedure mpifx_recv_z0, mpifx_recv_z1, mpifx_recv_z2, &
        & mpifx_recv_z3, mpifx_recv_z4, mpifx_recv_z5, mpifx_recv_z6
    module procedure mpifx_recv_h0, mpifx_recv_h1, mpifx_recv_h2, &
        & mpifx_recv_h3, mpifx_recv_h4, mpifx_recv_h5, mpifx_recv_h6
  end interface mpifx_recv


contains

  _subroutine_mpifx_recv(i0, integer, , 1, MPI_INTEGER)
  _subroutine_mpifx_recv(i1, integer, (:), size(msg), MPI_INTEGER)
  _subroutine_mpifx_recv(i2, integer, (:,:), size(msg), MPI_INTEGER)
  _subroutine_mpifx_recv(i3, integer, (:,:,:), size(msg), MPI_INTEGER)
  _subroutine_mpifx_recv(i4, integer, (:,:,:,:), size(msg), MPI_INTEGER)
  _subroutine_mpifx_recv(i5, integer, (:,:,:,:,:), size(msg), MPI_INTEGER)
  _subroutine_mpifx_recv(i6, integer, (:,:,:,:,:,:), size(msg), MPI_INTEGER)

  _subroutine_mpifx_recv(l0, logical, , 1, MPI_LOGICAL)
  _subroutine_mpifx_recv(l1, logical, (:), size(msg), MPI_LOGICAL)
  _subroutine_mpifx_recv(l2, logical, (:,:), size(msg), MPI_LOGICAL)
  _subroutine_mpifx_recv(l3, logical, (:,:,:), size(msg), MPI_LOGICAL)
  _subroutine_mpifx_recv(l4, logical, (:,:,:,:), size(msg), MPI_LOGICAL)
  _subroutine_mpifx_recv(l5, logical, (:,:,:,:,:), size(msg), MPI_LOGICAL)
  _subroutine_mpifx_recv(l6, logical, (:,:,:,:,:,:), size(msg), MPI_LOGICAL)

  _subroutine_mpifx_recv(s0, real(sp), , 1, MPI_REAL)
  _subroutine_mpifx_recv(s1, real(sp), (:), size(msg), MPI_REAL)
  _subroutine_mpifx_recv(s2, real(sp), (:,:), size(msg), MPI_REAL)
  _subroutine_mpifx_recv(s3, real(sp), (:,:,:), size(msg), MPI_REAL)
  _subroutine_mpifx_recv(s4, real(sp), (:,:,:,:), size(msg), MPI_REAL)
  _subroutine_mpifx_recv(s5, real(sp), (:,:,:,:,:), size(msg), MPI_REAL)
  _subroutine_mpifx_recv(s6, real(sp), (:,:,:,:,:,:), size(msg), MPI_REAL)

  _subroutine_mpifx_recv(d0, real(dp), , 1,
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_recv(d1, real(dp), (:), size(msg),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_recv(d2, real(dp), (:,:), size(msg),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_recv(d3, real(dp), (:,:,:), size(msg),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_recv(d4, real(dp), (:,:,:,:), size(msg),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_recv(d5, real(dp), (:,:,:,:,:), size(msg),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_recv(d6, real(dp), (:,:,:,:,:,:), size(msg),
      MPI_DOUBLE_PRECISION)

  _subroutine_mpifx_recv(c0, complex(sp), , 1, MPI_COMPLEX)
  _subroutine_mpifx_recv(c1, complex(sp), (:), size(msg), MPI_COMPLEX)
  _subroutine_mpifx_recv(c2, complex(sp), (:,:), size(msg), MPI_COMPLEX)
  _subroutine_mpifx_recv(c3, complex(sp), (:,:,:), size(msg), MPI_COMPLEX)
  _subroutine_mpifx_recv(c4, complex(sp), (:,:,:,:), size(msg), MPI_COMPLEX)
  _subroutine_mpifx_recv(c5, complex(sp), (:,:,:,:,:), size(msg), MPI_COMPLEX)
  _subroutine_mpifx_recv(c6, complex(sp), (:,:,:,:,:,:), size(msg), MPI_COMPLEX)

  _subroutine_mpifx_recv(z0, complex(dp), , 1,
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_recv(z1, complex(dp), (:), size(msg),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_recv(z2, complex(dp), (:,:), size(msg),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_recv(z3, complex(dp), (:,:,:), size(msg),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_recv(z4, complex(dp), (:,:,:,:), size(msg),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_recv(z5, complex(dp), (:,:,:,:,:), size(msg),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_recv(z6, complex(dp), (:,:,:,:,:,:), size(msg),
      MPI_DOUBLE_COMPLEX)

  _subroutine_mpifx_recv(h0, character(*), , len(msg),
      MPI_CHARACTER)
  _subroutine_mpifx_recv(h1, character(*), (:), size(msg) * len(msg), 
      MPI_CHARACTER)
  _subroutine_mpifx_recv(h2, character(*), (:,:), size(msg) * len(msg),
      MPI_CHARACTER)
  _subroutine_mpifx_recv(h3, character(*), (:,:,:), size(msg) * len(msg),
      MPI_CHARACTER)
  _subroutine_mpifx_recv(h4, character(*), (:,:,:,:), size(msg) * len(msg),
      MPI_CHARACTER)
  _subroutine_mpifx_recv(h5, character(*), (:,:,:,:,:), size(msg) * len(msg),
      MPI_CHARACTER)
  _subroutine_mpifx_recv(h6, character(*), (:,:,:,:,:,:), size(msg) * len(msg),
      MPI_CHARACTER)

end module mpifx_recv_module
