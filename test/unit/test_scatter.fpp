#:include "fortuno_mpi.fypp"

module test_scatter
  use libmpifx_module, only : mpifx_comm, mpifx_scatter
  use fortuno_mpi, only : all_equal, global_comm_id, suite => mpi_suite_item, test_list, is_equal
  $:FORTUNO_MPI_IMPORTS()
  implicit none

contains

  $:TEST("I1_to_I0")
    type(mpifx_comm) :: mycomm
    integer, allocatable :: send1(:)
    integer :: recv0 = 0
    integer :: ii

    call mycomm%init(global_comm_id())
    if (mycomm%lead) then
      allocate(send1(mycomm%size), source = 0)
      send1(:) = [ (ii, ii = 1, size(send1)) ]
    else
      allocate(send1(0), source = 0)
    end if
    call mpifx_scatter(mycomm, send1, recv0)

    @:ASSERT(is_equal(recv0, mycomm%rank + 1))
  $:END_TEST()

  $:TEST("I1_to_I1")
    type(mpifx_comm) :: mycomm
    integer, allocatable :: send1(:)
    integer, allocatable :: recv1(:)
    integer :: ii

    call mycomm%init(global_comm_id())
    if (mycomm%lead) then
      allocate(send1(2 * mycomm%size), source = 0)
      send1(:) = [ (ii, ii = 1, size(send1)) ]
    end if
    allocate(recv1(2), source = 0)
    call mpifx_scatter(mycomm, send1, recv1)

    @:ASSERT(all_equal(recv1, [2*mycomm%rank + 1, 2*mycomm%rank + 2]))
  $:END_TEST()

  $:TEST("I2_to_I1")
    type(mpifx_comm) :: mycomm
    integer, allocatable :: send2(:,:)
    integer, allocatable :: recv1(:)
    integer :: ii

    call mycomm%init(global_comm_id())
    if (mycomm%lead) then
      allocate(send2(2, mycomm%size), source = 0)
      send2(:,:) = reshape([ (ii, ii = 1, 2 * mycomm%size) ],  [ 2, mycomm%size ])
    else
      allocate(send2(0,0), source = 0)
    end if
    allocate(recv1(2), source = 0)
    call mpifx_scatter(mycomm, send2, recv1)

    @:ASSERT(all_equal(recv1, [2*mycomm%rank + 1, 2*mycomm%rank + 2]))
  $:END_TEST()

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("scatter", test_list([&
            $:TEST_ITEMS()
        ]))&
    ])
    @:STOP_ON_MISSING_TEST_ITEMS()

  end function tests

end module test_scatter
