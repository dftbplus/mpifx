#:include "fortuno_mpi.fypp"

module test_send_recv
  use libmpifx_module, only : mpifx_comm, mpifx_recv, mpifx_send
  use fortuno_mpi, only : global_comm_id, suite => mpi_suite_item, test_list, is_equal
  $:FORTUNO_MPI_IMPORTS()
  implicit none

contains

  $:TEST("send_to_lead")
    character(5) :: msg, expected
    type(mpifx_comm) :: mycomm
    integer :: source
    logical :: tPassed = .true.

    call mycomm%init(global_comm_id())
    if (.not. mycomm%lead) then
      write(msg, "(i0)") mycomm%rank
      call mpifx_send(mycomm, msg, mycomm%leadrank)
    else
      do source = 1, mycomm%size - 1
        write(expected, "(i0)") source
        call mpifx_recv(mycomm, msg, source)
        tPassed = tPassed .and. (msg == expected)
      end do
    end if

    @:ASSERT(tPassed)
  $:END_TEST()

  $:TEST("to_lower_neighbour")
    character(5) :: msg, expected
    type(mpifx_comm) :: mycomm
    integer :: destination
    integer :: source

    call mycomm%init(global_comm_id())
    if (mycomm%rank - 1 >= 0) then
        destination = mycomm%rank - 1
    else
      destination = mycomm%size - 1
    end if
    write(msg, "(i0)") mycomm%rank
    call mpifx_send(mycomm, msg, destination)

    if (mycomm%rank == mycomm%size - 1) then
      source = 0
    else
      source = mycomm%rank + 1
    end if
    write(expected, "(i0)") source
    call mpifx_recv(mycomm, msg, source)

    @:ASSERT(msg == expected)
  $:END_TEST()

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("send_recv", test_list([&
            $:TEST_ITEMS()
        ]))&
    ])
    @:STOP_ON_MISSING_TEST_ITEMS()

  end function tests

end module test_send_recv
