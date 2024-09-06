#:include "fortuno_mpi.fypp"

module test_gatherv
  use libmpifx_module, only : mpifx_comm, mpifx_gatherv
  use fortuno_mpi, only : global_comm_id, suite => mpi_suite_item, test_list, is_equal
  $:FORTUNO_MPI_IMPORTS()
  implicit none

contains

  $:TEST("R1_to_R1")
    type(mpifx_comm) :: mycomm
    integer, parameter :: sp = kind(1.0)
    real(sp), allocatable :: send1(:)
    real(sp), allocatable :: recv1(:)
    integer, allocatable :: recvcounts(:)
    integer :: ii, nrecv

    call mycomm%init(global_comm_id())
    allocate(send1(mycomm%rank+1), source = 0.0_sp)
    send1 = real(mycomm%rank+1, sp)
    if (mycomm%lead) then
      ! recv1 size is 1+2+3+...+mycomm%size
      nrecv = mycomm%size*(mycomm%size+1)/2
      allocate(recv1(nrecv), source = 0.0_sp)
      allocate(recvcounts(mycomm%size), source = 0)
      do ii = 1, mycomm%size
        recvcounts(ii) = ii
      end do
    else
      allocate(recv1(0), source = 0.0_sp)
      allocate(recvcounts(0), source = 0)
    end if
    call mpifx_gatherv(mycomm, send1, recv1, recvcounts)

    if (mycomm%lead) then
      @:ASSERT(is_equal(nint(sum(recv1)), (2*mycomm%size**3+3*mycomm%size**2+mycomm%size)/6))
      @:ASSERT((abs(sum(recv1)-nint(sum(recv1))) < epsilon(1.0_sp)))
    else
      @:ASSERT(is_equal(nint(sum(recv1)), 0))
      @:ASSERT((abs(sum(recv1)-nint(sum(recv1))) < epsilon(1.0_sp)))
    end if
  $:END_TEST()

  $:TEST("R2_to_R2")
    type(mpifx_comm) :: mycomm
    integer, parameter :: sp = kind(1.0)
    real(sp), allocatable :: send2(:,:)
    real(sp), allocatable :: recv2(:,:)
    integer, allocatable :: recvcounts(:)
    integer :: ii, nrecv, nCol

    call mycomm%init(global_comm_id())
    nCol = 10
    allocate(send2(nCol, mycomm%rank+1), source = 0.0_sp)
    send2 = real(mycomm%rank + 1, sp)
    if (mycomm%lead) then
      ! recv1 size is 1+2+3+...+mycomm%size
      nrecv = mycomm%size*(mycomm%size+1)/2
      allocate(recv2(nCol, nrecv), source = 0.0_sp)
      allocate(recvcounts(mycomm%size), source = 0)
      do ii = 1, mycomm%size
        recvcounts(ii) = nCol*ii
      end do
    else
      allocate(recv2(0,0), source = 0.0_sp)
      allocate(recvcounts(0), source = 0)
    end if
    call mpifx_gatherv(mycomm, send2, recv2, recvcounts)

    if (mycomm%lead) then
      @:ASSERT(is_equal(nint(sum(recv2)), nCol*mycomm%size*(mycomm%size+1)*(2*mycomm%size+1)/6))
      @:ASSERT((abs(sum(recv2)-nint(sum(recv2))) < epsilon(1.0_sp)))
    else
      @:ASSERT(is_equal(nint(sum(recv2)), 0))
      @:ASSERT((abs(sum(recv2)-nint(sum(recv2))) < epsilon(1.0_sp)))
    end if
  $:END_TEST()

  $:TEST("R0_to_R1")
    ! R0 -> R1 with specified receive pattern including gaps
    type(mpifx_comm) :: mycomm
    integer, parameter :: sp = kind(1.0)
    real(sp), allocatable :: recv1(:)
    real(sp) :: send0
    integer, allocatable :: recvcounts(:)
    integer, allocatable :: displs(:)
    integer :: ii, nrecv

    call mycomm%init(global_comm_id())
    send0 = real(mycomm%rank + 1, sp)
    if (mycomm%lead) then
      nrecv = mycomm%size
      allocate(recv1(2*nrecv), source = 0.0_sp)
      allocate(recvcounts(mycomm%size), source = 1)
      allocate(displs(mycomm%size), source = 0)
      ! set a non trivial displs vector
      do ii = 1, mycomm%size
        displs(ii) = 2*ii-1
      end do
      ! mark untouched elements
      recv1 = -1
    else
      allocate(recv1(0), source = 0.0_sp)
      allocate(recvcounts(0), source = 0)
      allocate(displs(0), source = 0)
    end if
    call mpifx_gatherv(mycomm, send0, recv1, recvcounts, displs)

    if (mycomm%lead) then
      @:ASSERT(is_equal(nint(sum(recv1)), (mycomm%size*(mycomm%size+1))/2 - mycomm%size))
      @:ASSERT((abs(sum(recv1)-nint(sum(recv1))) < epsilon(1.0_sp)))
    else
      @:ASSERT(is_equal(nint(sum(recv1)), 0))
      @:ASSERT((abs(sum(recv1)-nint(sum(recv1))) < epsilon(1.0_sp)))
    end if
  $:END_TEST()

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("gatherv", test_list([&
            $:TEST_ITEMS()
        ]))&
    ])
    @:STOP_ON_MISSING_TEST_ITEMS()

  end function tests

end module test_gatherv
