#:include "fortuno_mpi.fypp"

module test_comm_split
  use libmpifx_module, only : mpifx_comm
  use fortuno_mpi, only : global_comm_id, suite => mpi_suite_item, test_list, is_equal
  $:FORTUNO_MPI_IMPORTS()
  implicit none

contains

  $:TEST("split_group")
    type(mpifx_comm) :: allproc, groupproc
    integer :: groupsize, mygroup

    call allproc%init(global_comm_id())
    groupsize = nint(real(allproc%size / 2))
    mygroup = allproc%rank / groupsize
    call allproc%split(mygroup, allproc%rank, groupproc)

    if (allproc%rank < groupsize) then
      @:ASSERT(is_equal(mygroup, 0))
      @:ASSERT(is_equal(allproc%rank, groupproc%rank))
    else
      @:ASSERT(is_equal(mygroup, 1))
      @:ASSERT(is_equal(allproc%rank, groupproc%rank + groupsize))
    end if
  $:END_TEST()

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("comm_split", test_list([&
            $:TEST_ITEMS()
        ]))&
    ])
    @:STOP_ON_MISSING_TEST_ITEMS()

  end function tests

end module test_comm_split
