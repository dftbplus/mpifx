#:include "fortuno_mpi.fypp"

module test_bcast
  use libmpifx_module, only : mpifx_comm, mpifx_barrier, mpifx_bcast
  use fortuno_mpi, only : all_close, global_comm_id, suite => mpi_suite_item, test_list, this_rank
  $:FORTUNO_MPI_IMPORTS()
  implicit none

  integer, parameter :: dp = kind(1.0d0)

contains


  ! GIVEN zero initialized buffer on all ranks
  ! WHEN lead node broadcasts a rank one array of integers
  ! THEN buffer on all ranks contains that message
  $:TEST("integer_r1")
    integer, parameter :: msg(3) = [1, 2, 3]
    type(mpifx_comm) :: mycomm
    integer :: buffer(size(msg))

    call mycomm%init(global_comm_id())
    buffer(:) = 0
    if (mycomm%lead) buffer(:) = msg
    call mpifx_bcast(mycomm, buffer)
    @:ASSERT(all(buffer == msg))
  $:END_TEST()


  ! GIVEN zero initialized buffer on all ranks
  ! WHEN lead node broadcasts a rank one array of logicals
  ! THEN buffer on all ranks contains that message
  $:TEST("logical_r1")
    logical, parameter :: msg(3) = [.true., .false., .true.]
    type(mpifx_comm) :: mycomm
    logical :: buffer(size(msg))

    call mycomm%init(global_comm_id())
    buffer(:) = .false.
    if (mycomm%lead) buffer(:) = msg
    call mpifx_bcast(mycomm, buffer)
    @:ASSERT(all(buffer .eqv. msg))
  $:END_TEST()


  ! GIVEN zero initialized buffer on all ranks
  ! WHEN lead node broadcasts a rank two array of reals
  ! THEN buffer on all ranks contains that message
  $:TEST("real_r2")
    real(dp), parameter :: msg(2, 2) = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2])
    real(dp), parameter :: tol = 10.0_dp * epsilon(1.0_dp)
    type(mpifx_comm) :: mycomm
    real(dp) :: buffer(size(msg, dim=1), size(msg, dim=2))

    call mycomm%init(global_comm_id())
    buffer(:,:) = 0.0_dp
    if (mycomm%lead) buffer(:,:) = msg
    call mpifx_bcast(mycomm, buffer)
    @:ASSERT(all_close(buffer, msg, rtol=tol))
  $:END_TEST()


  ! GIVEN zero initialized buffer on all ranks
  ! WHEN lead node broadcasts a complex scalar
  ! THEN buffer on all ranks contains that message
  $:TEST("complex_r0")
    complex(dp), parameter :: msg = (-1.0_dp, 1.0_dp)
    real(dp), parameter :: tol = 10.0_dp * epsilon(1.0_dp)
    type(mpifx_comm) :: mycomm
    complex(dp) :: buffer

    call mycomm%init(global_comm_id())
    buffer = (0.0_dp, 0.0_dp)
    if (mycomm%lead) buffer = msg
    call mpifx_bcast(mycomm, buffer)
    @:ASSERT(abs(buffer - msg) < tol)
  $:END_TEST()


  ! GIVEN zero initialized buffer on all ranks
  ! WHEN lead node broadcasts a character string
  ! THEN buffer on all ranks contains that message
  $:TEST("character_string")
    character(5), parameter :: msg = "hello"
    type(mpifx_comm) :: mycomm
    character(len(msg)) :: buffer

    call mycomm%init(global_comm_id())
    buffer = repeat(" ", len(buffer))
    if (mycomm%lead) buffer = msg
    call mpifx_bcast(mycomm, buffer)
    @:ASSERT(buffer == msg)
  $:END_TEST()


  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("bcast", test_list([&
            $:TEST_ITEMS()
        ]))&
    ])
    @:STOP_ON_MISSING_TEST_ITEMS()

  end function tests

end module test_bcast
