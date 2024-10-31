#:include "fortuno_mpi.fypp"

module test_allreduce
  use mpi, only : MPI_SUM, MPI_PROD
  use libmpifx_module, only : mpifx_comm, mpifx_allreduce, mpifx_allreduceip
  use fortuno_mpi, only : all_close, global_comm_id, suite => mpi_suite_item, test_list, is_equal
  $:FORTUNO_MPI_IMPORTS()
  implicit none

contains

  $:TEST("scalar_sum")
    type(mpifx_comm) :: mycomm
    integer :: vali0, resvali0

    call mycomm%init(global_comm_id())
    vali0 = mycomm%rank * 2
    call mpifx_allreduce(mycomm, vali0, resvali0, MPI_SUM)

    @:ASSERT(is_equal(resvali0, mycomm%size * (mycomm%size-1)))
  $:END_TEST()

  $:TEST("array_prod")
    integer, parameter :: dp = kind(1.0d0)
    integer, parameter :: max_valid_rank = 9
    type(mpifx_comm) :: mycomm
    real(dp) :: valr(3), resvalr(3)
    integer :: max_none_one_rank

    call mycomm%init(global_comm_id())

    if (mycomm%rank <= max_valid_rank) then
      valr(:) = [ real(mycomm%rank + 1, dp) * 1.2, &
          & real(mycomm%rank + 1, dp) * 4.3, real(mycomm%rank + 1, dp) * 3.8 ]
    else
      ! prevents the product from getting to large
      valr(:) = [ real(1, dp), real(1, dp), real(1, dp) ]
    end if
    call mpifx_allreduce(mycomm, valr, resvalr, MPI_PROD)

    if (mycomm%size <= max_valid_rank + 1) then
      max_none_one_rank = mycomm%size
    else
      max_none_one_rank = max_valid_rank + 1
    end if

    @:ASSERT(all_close(resvalr, [gamma(real(max_none_one_rank + 1, kind=dp)) * (1.2)**(max_none_one_rank), &
        & gamma(real(max_none_one_rank + 1, kind=dp)) * (4.3)**(max_none_one_rank), &
        & gamma(real(max_none_one_rank + 1, kind=dp)) * (3.8)**(max_none_one_rank)], rtol=1e-6_dp))
  $:END_TEST()

  $:TEST("ip_sum")
    integer, parameter :: dp = kind(1.0d0)
    type(mpifx_comm) :: mycomm
    real(dp) :: resvalr(3)

    call mycomm%init(global_comm_id())
    resvalr(:) = [ real(mycomm%rank + 1, dp) * 1.2, &
        & real(mycomm%rank + 1, dp) * 4.3, real(mycomm%rank + 1, dp) * 3.8 ]
    call mpifx_allreduceip(mycomm, resvalr, MPI_SUM)

    @:ASSERT(all_close(resvalr, [mycomm%size * (mycomm%size + 1) / 2 * 1.2_dp, &
        & mycomm%size * (mycomm%size + 1) / 2 * 4.3_dp, mycomm%size * (mycomm%size + 1) / 2 * 3.8_dp], &
        & rtol=1e-7_dp))
  $:END_TEST()

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("allreduce", test_list([&
            $:TEST_ITEMS()
        ]))&
    ])
    @:STOP_ON_MISSING_TEST_ITEMS()

  end function tests

end module test_allreduce
