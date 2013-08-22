include(mpifx_send.m4)

!> Contains wrapper for \c MPI_SEND
module mpifx_send_module
  use mpifx_common_module
  implicit none
  private

  public :: mpifx_send


  !> Sends a message to a given node.
  !!
  !! \details All functions have the same argument list only differing in the
  !! type and rank of the second argument. The second argument can be of
  !! type integer (i), real (s), double precision (d), complex (c), 
  !! double complex (z), logical (b) and character (h). Its rank can vary from
  !! zero (scalar) up to the maximum rank.
  !!
  !! \see MPI documentation (\c MPI_SEND)
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
  !!       write(msg, "(A,I0,A)") "Hello from process ", mycomm%rank, "!"
  !!       call mpifx_send(mycomm, msg, mycomm%masterrank)
  !!     else
  !!       write(*, "(A)") "Master node:"
  !!       do source = 1, mycomm%size - 1
  !!         call mpifx_recv(mycomm, msg, source)
  !!         write(*,"(A,A)") "Message received: ", trim(msg)
  !!       end do
  !!     end if
  !!     call mpifx_finalize()
  !!
  !!   end program hello
  !!
  interface mpifx_send
    module procedure mpifx_send_i0, mpifx_send_i1, mpifx_send_i2, &
        & mpifx_send_i3, mpifx_send_i4, mpifx_send_i5, mpifx_send_i6
    module procedure mpifx_send_l0, mpifx_send_l1, mpifx_send_l2, &
        & mpifx_send_l3, mpifx_send_l4, mpifx_send_l5, mpifx_send_l6
    module procedure mpifx_send_s0, mpifx_send_s1, mpifx_send_s2, &
        & mpifx_send_s3, mpifx_send_s4, mpifx_send_s5, mpifx_send_s6
    module procedure mpifx_send_d0, mpifx_send_d1, mpifx_send_d2, &
        & mpifx_send_d3, mpifx_send_d4, mpifx_send_d5, mpifx_send_d6
    module procedure mpifx_send_c0, mpifx_send_c1, mpifx_send_c2, &
        & mpifx_send_c3, mpifx_send_c4, mpifx_send_c5, mpifx_send_c6
    module procedure mpifx_send_z0, mpifx_send_z1, mpifx_send_z2, &
        & mpifx_send_z3, mpifx_send_z4, mpifx_send_z5, mpifx_send_z6
    module procedure mpifx_send_h0, mpifx_send_h1, mpifx_send_h2, &
        & mpifx_send_h3, mpifx_send_h4, mpifx_send_h5, mpifx_send_h6
  end interface mpifx_send


contains

  _subroutine_mpifx_send(i0, integer, , 1, MPI_INTEGER)
  _subroutine_mpifx_send(i1, integer, (:), size(msg), MPI_INTEGER)
  _subroutine_mpifx_send(i2, integer, (:,:), size(msg), MPI_INTEGER)
  _subroutine_mpifx_send(i3, integer, (:,:,:), size(msg), MPI_INTEGER)
  _subroutine_mpifx_send(i4, integer, (:,:,:,:), size(msg), MPI_INTEGER)
  _subroutine_mpifx_send(i5, integer, (:,:,:,:,:), size(msg), MPI_INTEGER)
  _subroutine_mpifx_send(i6, integer, (:,:,:,:,:,:), size(msg), MPI_INTEGER)

  _subroutine_mpifx_send(l0, logical, , 1, MPI_LOGICAL)
  _subroutine_mpifx_send(l1, logical, (:), size(msg), MPI_LOGICAL)
  _subroutine_mpifx_send(l2, logical, (:,:), size(msg), MPI_LOGICAL)
  _subroutine_mpifx_send(l3, logical, (:,:,:), size(msg), MPI_LOGICAL)
  _subroutine_mpifx_send(l4, logical, (:,:,:,:), size(msg), MPI_LOGICAL)
  _subroutine_mpifx_send(l5, logical, (:,:,:,:,:), size(msg), MPI_LOGICAL)
  _subroutine_mpifx_send(l6, logical, (:,:,:,:,:,:), size(msg), MPI_LOGICAL)

  _subroutine_mpifx_send(s0, real(sp), , 1, MPI_REAL)
  _subroutine_mpifx_send(s1, real(sp), (:), size(msg), MPI_REAL)
  _subroutine_mpifx_send(s2, real(sp), (:,:), size(msg), MPI_REAL)
  _subroutine_mpifx_send(s3, real(sp), (:,:,:), size(msg), MPI_REAL)
  _subroutine_mpifx_send(s4, real(sp), (:,:,:,:), size(msg), MPI_REAL)
  _subroutine_mpifx_send(s5, real(sp), (:,:,:,:,:), size(msg), MPI_REAL)
  _subroutine_mpifx_send(s6, real(sp), (:,:,:,:,:,:), size(msg), MPI_REAL)

  _subroutine_mpifx_send(d0, real(dp), , 1,
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_send(d1, real(dp), (:), size(msg),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_send(d2, real(dp), (:,:), size(msg),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_send(d3, real(dp), (:,:,:), size(msg),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_send(d4, real(dp), (:,:,:,:), size(msg),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_send(d5, real(dp), (:,:,:,:,:), size(msg),
      MPI_DOUBLE_PRECISION)
  _subroutine_mpifx_send(d6, real(dp), (:,:,:,:,:,:), size(msg),
      MPI_DOUBLE_PRECISION)

  _subroutine_mpifx_send(c0, complex(sp), , 1, MPI_COMPLEX)
  _subroutine_mpifx_send(c1, complex(sp), (:), size(msg), MPI_COMPLEX)
  _subroutine_mpifx_send(c2, complex(sp), (:,:), size(msg), MPI_COMPLEX)
  _subroutine_mpifx_send(c3, complex(sp), (:,:,:), size(msg), MPI_COMPLEX)
  _subroutine_mpifx_send(c4, complex(sp), (:,:,:,:), size(msg), MPI_COMPLEX)
  _subroutine_mpifx_send(c5, complex(sp), (:,:,:,:,:), size(msg), MPI_COMPLEX)
  _subroutine_mpifx_send(c6, complex(sp), (:,:,:,:,:,:), size(msg), MPI_COMPLEX)

  _subroutine_mpifx_send(z0, complex(dp), , 1,
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_send(z1, complex(dp), (:), size(msg),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_send(z2, complex(dp), (:,:), size(msg),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_send(z3, complex(dp), (:,:,:), size(msg),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_send(z4, complex(dp), (:,:,:,:), size(msg),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_send(z5, complex(dp), (:,:,:,:,:), size(msg),
      MPI_DOUBLE_COMPLEX)
  _subroutine_mpifx_send(z6, complex(dp), (:,:,:,:,:,:), size(msg),
      MPI_DOUBLE_COMPLEX)

  _subroutine_mpifx_send(h0, character(*), , len(msg),
      MPI_CHARACTER)
  _subroutine_mpifx_send(h1, character(*), (:), size(msg) * len(msg), 
      MPI_CHARACTER)
  _subroutine_mpifx_send(h2, character(*), (:,:), size(msg) * len(msg),
      MPI_CHARACTER)
  _subroutine_mpifx_send(h3, character(*), (:,:,:), size(msg) * len(msg),
      MPI_CHARACTER)
  _subroutine_mpifx_send(h4, character(*), (:,:,:,:), size(msg) * len(msg),
      MPI_CHARACTER)
  _subroutine_mpifx_send(h5, character(*), (:,:,:,:,:), size(msg) * len(msg),
      MPI_CHARACTER)
  _subroutine_mpifx_send(h6, character(*), (:,:,:,:,:,:), size(msg) * len(msg),
      MPI_CHARACTER)


end module mpifx_send_module
