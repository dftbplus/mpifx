#:include "fortuno_mpi.fypp"

module test_comm_split_type
  use mpi, only : MPI_COMM_TYPE_SHARED
  use libmpifx_module, only : mpifx_comm
  use fortuno_mpi, only : global_comm_id, suite => mpi_suite_item, test_list, is_equal
  $:FORTUNO_MPI_IMPORTS()
  implicit none

contains

  $:TEST("split_shared")
    type(mpifx_comm) :: allproc, splitproc

    call allproc%init(global_comm_id())
    call allproc%split_type(MPI_COMM_TYPE_SHARED, allproc%rank, splitproc)

    @:ASSERT(is_equal(allproc%rank, splitproc%rank))
  $:END_TEST()

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("comm_split_type", test_list([&
            $:TEST_ITEMS()
        ]))&
    ])
    @:STOP_ON_MISSING_TEST_ITEMS()

  end function tests

end module test_comm_split_type
