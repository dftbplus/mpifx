!> Test various patterns of allgather
#:include "fortuno_mpi.fypp"

module test_allgather
  use libmpifx_module, only : mpifx_comm, mpifx_allgather
  use fortuno_mpi, only : global_comm_id, suite => mpi_suite_item, test_list, is_equal
  $:FORTUNO_MPI_IMPORTS()
  implicit none

contains

  $:TEST("I0_to_I1")
    type(mpifx_comm) :: mycomm
    integer :: send0
    integer, allocatable :: recv1(:)

    call mycomm%init(global_comm_id())
    send0 = mycomm%rank * 2
    allocate(recv1(1 * mycomm%size), source = 0)
    call mpifx_allgather(mycomm, send0, recv1)

    @:ASSERT(is_equal(sum(recv1), mycomm%size * (mycomm%size-1)))
  $:END_TEST()

  $:TEST("I1_to_I1")
    type(mpifx_comm) :: mycomm
    integer, allocatable :: send1(:)
    integer, allocatable :: recv1(:)

    call mycomm%init(global_comm_id())
    allocate(send1(2), source = 0)
    allocate(recv1(size(send1) * mycomm%size), source = 0)
    send1(:) = [ mycomm%rank, mycomm%rank + 1 ]
    call mpifx_allgather(mycomm, send1, recv1)

    @:ASSERT(is_equal(sum(recv1), mycomm%size**2))
  $:END_TEST()

  $:TEST("I1_to_I2")
    type(mpifx_comm) :: mycomm
    integer, allocatable :: send1(:)
    integer, allocatable :: recv2(:,:)

    call mycomm%init(global_comm_id())
    allocate(send1(2), source = 0)
    allocate(recv2(size(send1), mycomm%size), source = 0)
    send1(:) = [ mycomm%rank, mycomm%rank + 1 ]
    call mpifx_allgather(mycomm, send1, recv2)

    @:ASSERT(is_equal(sum(recv2), mycomm%size**2))
  $:END_TEST()

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("allgather", test_list([&
            $:TEST_ITEMS()
        ]))&
    ])
    @:STOP_ON_MISSING_TEST_ITEMS()

  end function tests

end module test_allgather
