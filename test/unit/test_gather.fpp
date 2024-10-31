#:include "fortuno_mpi.fypp"

module test_gather
  use libmpifx_module, only : mpifx_comm, mpifx_gather
  use fortuno_mpi, only : global_comm_id, suite => mpi_suite_item, test_list, is_equal
  $:FORTUNO_MPI_IMPORTS()
  implicit none

contains

  $:TEST("I0_to_I1")
    type(mpifx_comm) :: mycomm
    integer :: send0
    integer, allocatable :: recv1(:)

    call mycomm%init(global_comm_id())
    send0 = mycomm%rank * 2   ! Arbitrary number to send
    if (mycomm%lead) then
      allocate(recv1(1 * mycomm%size), source = 0)
    else
      allocate(recv1(0), source = 0)
    end if
    call mpifx_gather(mycomm, send0, recv1)

    if (mycomm%lead) then
      @:ASSERT(is_equal(sum(recv1), mycomm%size * (mycomm%size-1)))
    else
      @:ASSERT(is_equal(sum(recv1), 0))
    end if
  $:END_TEST()

  $:TEST("I1_to_I1")
    type(mpifx_comm) :: mycomm
    integer, allocatable :: send1(:)
    integer, allocatable :: recv1(:)

    call mycomm%init(global_comm_id())
    allocate(send1(2), source = 0)
    if (mycomm%lead) then
      allocate(recv1(size(send1) * mycomm%size), source = 0)
    else
      allocate(recv1(0), source = 0)
    end if
    send1(:) = [ mycomm%rank, mycomm%rank + 1 ]  ! Arbitrary numbers to send
    call mpifx_gather(mycomm, send1, recv1)

    if (mycomm%lead) then
      @:ASSERT(is_equal(sum(recv1), mycomm%size**2))
    else
      @:ASSERT(is_equal(sum(recv1), 0))
    end if
  $:END_TEST()

  $:TEST("I1_to_I2")
    type(mpifx_comm) :: mycomm
    integer, allocatable :: send1(:)
    integer, allocatable :: recv2(:,:)

    call mycomm%init(global_comm_id())
    allocate(send1(2), source = 0)
    if (mycomm%lead) then
      allocate(recv2(size(send1), mycomm%size), source = 0)
    else
      allocate(recv2(0, 0), source = 0)
    end if
    send1(:) = [ mycomm%rank, mycomm%rank + 1 ]  ! Arbitrary numbers to send
    call mpifx_gather(mycomm, send1, recv2)

    if (mycomm%lead) then
      @:ASSERT(is_equal(sum(recv2), mycomm%size**2))
    else
      @:ASSERT(is_equal(sum(recv2), 0))
    end if
  $:END_TEST()

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("gather", test_list([&
            $:TEST_ITEMS()
        ]))&
    ])
    @:STOP_ON_MISSING_TEST_ITEMS()

  end function tests

end module test_gather
