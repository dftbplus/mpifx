#:include "fortuno_mpi.fypp"

module test_scatterv
  use libmpifx_module, only : mpifx_comm, mpifx_scatterv
  use fortuno_mpi, only : global_comm_id, suite => mpi_suite_item, test_list, is_equal
  $:FORTUNO_MPI_IMPORTS()
  implicit none

contains

  $:TEST("I1_to_I0")
    type(mpifx_comm) :: mycomm
    integer, allocatable :: send1(:)
    integer :: recv0 = 0
    integer, allocatable :: sendcount(:)
    integer :: ii

    call mycomm%init(global_comm_id())
    if (mycomm%lead) then
      allocate(sendcount(mycomm%size), source = 1)
      allocate(send1(mycomm%size), source = 0)
      send1(:) = [ (ii, ii = 1, size(send1)) ]
    else
      allocate(sendcount(0), source = 0)
      allocate(send1(0), source = 0)
    end if
    call mpifx_scatterv(mycomm, send1, sendcount, recv0)

    @:ASSERT(is_equal(recv0, mycomm%rank + 1))
  $:END_TEST()

  $:TEST("I1_to_I1")
    type(mpifx_comm) :: mycomm
    integer, allocatable :: send1(:)
    integer, allocatable :: recv1(:), sendcount(:)
    integer :: ii

    call mycomm%init(global_comm_id())
    if (mycomm%lead) then
      allocate(sendcount(mycomm%size), source = 2)
      allocate(send1(2 * mycomm%size), source = 0)
      send1(:) = [ (ii, ii = 1, size(send1)) ]
    else
      allocate(sendcount(0), source = 0)
      allocate(send1(0), source = 0)
    end if
    allocate(recv1(2), source = 0)
    call mpifx_scatterv(mycomm, send1, sendcount, recv1)

    @:ASSERT(is_equal(recv1(1), 2*mycomm%rank + 1))
    @:ASSERT(is_equal(recv1(2), 2*mycomm%rank + 2))
  $:END_TEST()

  $:TEST("I2_to_I1")
    type(mpifx_comm) :: mycomm
    integer, allocatable :: send2(:,:)
    integer :: recv0
    integer, allocatable :: recv1(:), sendcount(:)
    integer :: ii

    call mycomm%init(global_comm_id())
    if (mycomm%lead) then
      allocate(sendcount(mycomm%size), source = 2)
      allocate(send2(2, mycomm%size), source = 0)
      send2(:,:) = reshape([ (ii, ii = 1, 2 * mycomm%size) ],  [ 2, mycomm%size ])
    else
      allocate(sendcount(0), source = 0)
      allocate(send2(0,0), source = 0)
    end if
    allocate(recv1(2), source = 0)
    call mpifx_scatterv(mycomm, send2, sendcount, recv1)

    @:ASSERT(is_equal(recv1(1), 2*mycomm%rank + 1))
    @:ASSERT(is_equal(recv1(2), 2*mycomm%rank + 2))
  $:END_TEST()

  $:TEST("I1_to_I1_disp")
    type(mpifx_comm) :: mycomm
    integer, allocatable :: send1(:)
    integer, allocatable :: recv1(:), sendcount(:), displs(:)
    integer :: ii

    call mycomm%init(global_comm_id())
    if (mycomm%lead) then
      allocate(sendcount(mycomm%size), source = 1)
      allocate(send1(2 * mycomm%size), source = 0)
      send1(:) = [ (ii, ii = 1, size(send1)) ]
      allocate(displs(mycomm%size), source = 0)
      displs(:) = [ (ii, ii = 1, size(send1), 2) ]
    else
      allocate(sendcount(0), source = 0)
      allocate(send1(0), source = 0)
      allocate(displs(0), source = 0)
    end if
    allocate(recv1(1), source = 0)
    call mpifx_scatterv(mycomm, send1, sendcount, recv1, displs=displs)

    @:ASSERT(is_equal(recv1(1), 2*mycomm%rank + 2))
  $:END_TEST()

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("scatterv", test_list([&
            $:TEST_ITEMS()
        ]))&
    ])
    @:STOP_ON_MISSING_TEST_ITEMS()

  end function tests

end module test_scatterv
