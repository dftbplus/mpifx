#:include 'mpifx.fypp'
#:set TYPES = ALL_TYPES


#! ************************************************************************
#! *** mpifx_gatherv
#! ************************************************************************


#:def mpifx_gatherv_dr0_template(VAR1, VAR2, VAR3, VAR4, VAR5)
#!
  #!
  #! ${VAR1}$: subroutine suffix
  #! ${VAR2}$: send/recv buffer type
  #! ${VAR3}$: send/recv buffer rank specifier ("", (:), (:,:), etc.)
  #! ${VAR4}$: send/recv buffer rank (1, 2, etc.)
  #! ${VAR5}$: corresponding MPI type
  #!
  !> Gathers results of variable length on one process (type ${VAR1}$).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (undefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%masterrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_${VAR1}$(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    ${VAR2}$, intent(in) :: send${VAR3}$
    ${VAR2}$, intent(out) :: recv${VAR3}$
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: root0, error0, ii, locLast(1), aborterror
    integer, allocatable :: displs0(:)
    logical, allocatable :: testBuffer(:)

    @:inoptflags(root0, root, mycomm%masterrank)

    if (mycomm%rank == root0) then
      allocate(displs0(mycomm%size))
      if (present(displs)) then
        @:ASSERT(size(displs) == mycomm%size)
        displs0 = displs
        locLast = maxloc(displs0)
        @:ASSERT(size(recv) >= displs0(locLast(1)) + recvcounts(locLast(1)))
        ! test for overlapping regions being written to
        allocate(testBuffer(size(recv)))
        testBuffer = .false.
        do ii = 1, mycomm%size
          ! potentially in random order, so mark effected parts of the buffer
          if (any(testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1))) then
            write(*, "(A)") "Overlapping regions in mpifx_gatherv!"
            call mpi_abort(MPI_COMM_WORLD, -1, aborterror)
            if (aborterror /= 0) then
              write(*, "(A)") "Stopping code did not succeed, hope for the best."
            end if
          end if
          testBuffer(displs0(ii):displs0(ii)+recvcounts(ii)-1) = .true.
        end do
        deallocate(testBuffer)
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do
        @:ASSERT(sum(recvcounts) == size(recv))
      end if
    end if

    call mpi_gatherv(send, size(send), ${VAR5}$, recv, recvcounts, displs0, &
        & ${VAR5}$, root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_${VAR1}$", error)

  end subroutine mpifx_gatherv_${VAR1}$

#:enddef



#:def mpifx_gatherv_dr1_template(VAR1, VAR2, VAR3, VAR4, VAR5, VAR6, VAR7)
#!
  #!
  #! ${VAR1}$: subroutine suffix
  #! ${VAR2}$: send/recv buffer type
  #! ${VAR3}$: send buffer rank specifier ("", (:), (:,:), etc.)
  #! ${VAR4}$: send buffer size (1 or size(send))
  #! ${VAR5}$: recv buffer rank specifier ((:), (:,:), etc.)
  #! ${VAR6}$: recv buffers rank (1, 2, etc.)
  #! ${VAR7}$: corresponding MPI type
  #!
  !> Gathers results on one process (type ${VAR1}$).
  !!
  !! \param mycomm  MPI communicator.
  !! \param send  Quantity to be sent for gathering.
  !! \param recv  Received data on receive node (indefined on other nodes)
  !! \param recvcounts Counts of received data from each process
  !! \param displs Entry i specifies where to place data from process rank i-1
  !!               (default: computed from recvcounts assuming order with rank)
  !! \param root  Root process for the result (default: mycomm%masterrank)
  !! \param error  Error code on exit.
  !!
  subroutine mpifx_gatherv_${VAR1}$(mycomm, send, recv, recvcounts, displs, root, error)
    type(mpifx_comm), intent(in) :: mycomm
    ${VAR2}$, intent(in) :: send${VAR3}$
    ${VAR2}$, intent(out) :: recv${VAR5}$
    integer, intent(in) :: recvcounts(:)
    integer, intent(in), optional :: displs(:)
    integer, intent(in), optional :: root
    integer, intent(out), optional :: error

    integer :: ii, root0, error0
    integer, allocatable :: displs0(:)

    @:inoptflags(root0, root, mycomm%masterrank)

    if (mycomm%rank == root0) then
      @:ASSERT(size(recv) == sum(recvcounts))
      @:ASSERT(size(recv, dim=${VAR6}$) == mycomm%size)
      allocate(displs0(mycomm%size))
      if (present(displs)) then
        @:ASSERT(size(displs) == mycomm%size)
        displs0 = displs
      else
        displs0(1) = 0
        do ii = 2, mycomm%size
          displs0(ii) = displs0(ii-1) + recvcounts(ii-1)
        end do
      end if
    end if

    call mpi_gatherv(send, ${VAR4}$, ${VAR7}$, recv, recvcounts, displs0, &
         & ${VAR7}$,  root0, mycomm%id, error0)

    call handle_errorflag(error0, "MPI_GATHERV in mpifx_gatherv_${VAR1}$", error)

  end subroutine mpifx_gatherv_${VAR1}$

#:enddef


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
        & mpifx_gatherv_i0i1
    module procedure &
        & mpifx_gatherv_s1s1, mpifx_gatherv_s2s2, mpifx_gatherv_s3s3, &
        & mpifx_gatherv_s4s4, mpifx_gatherv_s5s5, mpifx_gatherv_s6s6
    module procedure &
        & mpifx_gatherv_s0s1
    module procedure &
        & mpifx_gatherv_d1d1, mpifx_gatherv_d2d2, mpifx_gatherv_d3d3, &
        & mpifx_gatherv_d4d4, mpifx_gatherv_d5d5, mpifx_gatherv_d6d6
    module procedure &
        & mpifx_gatherv_d0d1
    module procedure &
        & mpifx_gatherv_c1c1, mpifx_gatherv_c2c2, mpifx_gatherv_c3c3, &
        & mpifx_gatherv_c4c4, mpifx_gatherv_c5c5, mpifx_gatherv_c6c6
    module procedure &
        & mpifx_gatherv_c0c1
    module procedure &
        & mpifx_gatherv_z1z1, mpifx_gatherv_z2z2, mpifx_gatherv_z3z3, &
        & mpifx_gatherv_z4z4, mpifx_gatherv_z5z5, mpifx_gatherv_z6z6
    module procedure &
        & mpifx_gatherv_z0z1
    module procedure &
        & mpifx_gatherv_l1l1, mpifx_gatherv_l2l2, mpifx_gatherv_l3l3, &
        & mpifx_gatherv_l4l4, mpifx_gatherv_l5l5, mpifx_gatherv_l6l6
    module procedure &
        & mpifx_gatherv_l0l1
  end interface mpifx_gatherv


contains

  @:mpifx_gatherv_dr0_template(i1i1, integer, (:), 1, MPI_INTEGER)
  @:mpifx_gatherv_dr0_template(i2i2, integer, (:,:), 2, MPI_INTEGER)
  @:mpifx_gatherv_dr0_template(i3i3, integer, (:,:,:), 3, MPI_INTEGER)
  @:mpifx_gatherv_dr0_template(i4i4, integer, (:,:,:,:), 4, MPI_INTEGER)
  @:mpifx_gatherv_dr0_template(i5i5, integer, (:,:,:,:,:), 5, MPI_INTEGER)
  @:mpifx_gatherv_dr0_template(i6i6, integer, (:,:,:,:,:,:), 6, MPI_INTEGER)


  @:mpifx_gatherv_dr0_template(s1s1, real(sp), (:), 1, MPI_REAL)
  @:mpifx_gatherv_dr0_template(s2s2, real(sp), (:,:), 2, MPI_REAL)
  @:mpifx_gatherv_dr0_template(s3s3, real(sp), (:,:,:), 3, MPI_REAL)
  @:mpifx_gatherv_dr0_template(s4s4, real(sp), (:,:,:,:), 4, MPI_REAL)
  @:mpifx_gatherv_dr0_template(s5s5, real(sp), (:,:,:,:,:), 5, MPI_REAL)
  @:mpifx_gatherv_dr0_template(s6s6, real(sp), (:,:,:,:,:,:), 6, MPI_REAL)


  @:mpifx_gatherv_dr0_template(d1d1, real(dp), (:), 1, MPI_DOUBLE_PRECISION)
  @:mpifx_gatherv_dr0_template(d2d2, real(dp), (:,:), 2, MPI_DOUBLE_PRECISION)
  @:mpifx_gatherv_dr0_template(d3d3, real(dp), (:,:,:), 3, MPI_DOUBLE_PRECISION)
  @:mpifx_gatherv_dr0_template(d4d4, real(dp), (:,:,:,:), 4, MPI_DOUBLE_PRECISION)
  @:mpifx_gatherv_dr0_template(d5d5, real(dp), (:,:,:,:,:), 5, MPI_DOUBLE_PRECISION)
  @:mpifx_gatherv_dr0_template(d6d6, real(dp), (:,:,:,:,:,:), 6, MPI_DOUBLE_PRECISION)


  @:mpifx_gatherv_dr0_template(c1c1, complex(sp), (:), 1, MPI_COMPLEX)
  @:mpifx_gatherv_dr0_template(c2c2, complex(sp), (:,:), 2, MPI_COMPLEX)
  @:mpifx_gatherv_dr0_template(c3c3, complex(sp), (:,:,:), 3, MPI_COMPLEX)
  @:mpifx_gatherv_dr0_template(c4c4, complex(sp), (:,:,:,:), 4, MPI_COMPLEX)
  @:mpifx_gatherv_dr0_template(c5c5, complex(sp), (:,:,:,:,:), 5, MPI_COMPLEX)
  @:mpifx_gatherv_dr0_template(c6c6, complex(sp), (:,:,:,:,:,:), 6, MPI_COMPLEX)


  @:mpifx_gatherv_dr0_template(z1z1, complex(dp), (:), 1, MPI_DOUBLE_COMPLEX)
  @:mpifx_gatherv_dr0_template(z2z2, complex(dp), (:,:), 2, MPI_DOUBLE_COMPLEX)
  @:mpifx_gatherv_dr0_template(z3z3, complex(dp), (:,:,:), 3, MPI_DOUBLE_COMPLEX)
  @:mpifx_gatherv_dr0_template(z4z4, complex(dp), (:,:,:,:), 4, MPI_DOUBLE_COMPLEX)
  @:mpifx_gatherv_dr0_template(z5z5, complex(dp), (:,:,:,:,:), 5, MPI_DOUBLE_COMPLEX)
  @:mpifx_gatherv_dr0_template(z6z6, complex(dp), (:,:,:,:,:,:), 6, MPI_DOUBLE_COMPLEX)


  @:mpifx_gatherv_dr0_template(l1l1, logical, (:), 1, MPI_LOGICAL)
  @:mpifx_gatherv_dr0_template(l2l2, logical, (:,:), 2, MPI_LOGICAL)
  @:mpifx_gatherv_dr0_template(l3l3, logical, (:,:,:), 3, MPI_LOGICAL)
  @:mpifx_gatherv_dr0_template(l4l4, logical, (:,:,:,:), 4, MPI_LOGICAL)
  @:mpifx_gatherv_dr0_template(l5l5, logical, (:,:,:,:,:), 5, MPI_LOGICAL)
  @:mpifx_gatherv_dr0_template(l6l6, logical, (:,:,:,:,:,:), 6, MPI_LOGICAL)

  @:mpifx_gatherv_dr1_template(i0i1, integer, , 1, (:), 1, MPI_INTEGER)
  @:mpifx_gatherv_dr1_template(s0s1, real(sp), , 1, (:), 1, MPI_REAL)
  @:mpifx_gatherv_dr1_template(d0d1, real(dp), , 1, (:), 1, MPI_DOUBLE_PRECISION)
  @:mpifx_gatherv_dr1_template(c0c1, complex(sp), , 1, (:), 1, MPI_COMPLEX)
  @:mpifx_gatherv_dr1_template(z0z1, complex(dp), , 1, (:), 1, MPI_DOUBLE_COMPLEX)
  @:mpifx_gatherv_dr1_template(l0l1, logical, , 1, (:), 1, MPI_LOGICAL)

end module mpifx_gatherv_module
