#:include "fortuno_mpi.fypp"

module test_reduce
  use mpi, only : MPI_SUM, MPI_PROD
  use libmpifx_module, only : mpifx_comm, mpifx_reduce, mpifx_reduceip
  use fortuno_mpi, only : global_comm_id, suite => mpi_suite_item, test_list, is_equal
  $:FORTUNO_MPI_IMPORTS()
  implicit none

contains

  $:TEST("scalar_sum")
    type(mpifx_comm) :: mycomm
    integer :: vali0, resvali0

    call mycomm%init(global_comm_id())
    vali0 = mycomm%rank * 2
    call mpifx_reduce(mycomm, vali0, resvali0, MPI_SUM)

    if (mycomm%lead) then
      @:ASSERT(is_equal(resvali0, mycomm%size * (mycomm%size-1)))
    else
      @:ASSERT(is_equal(resvali0, 0))
    end if
  $:END_TEST()

  $:TEST("array_prod")
    integer, parameter :: dp = kind(1.0d0)
    type(mpifx_comm) :: mycomm
    real(dp) :: valr(3), resvalr(3)
    integer :: max_none_one_rank, max_valid_rank

    max_valid_rank = 9
    call mycomm%init(global_comm_id())
    if (mycomm%rank <= max_valid_rank) then
      valr(:) = [ real(mycomm%rank + 1, dp) * 1.2, &
          & real(mycomm%rank + 1, dp) * 4.3, real(mycomm%rank + 1, dp) * 3.8 ]
    else
      ! prevents the product from getting to large
      valr(:) = [ real(1, dp), real(1, dp), real(1, dp) ]
    end if
    resvalr(:) = 0.0_dp
    call mpifx_reduce(mycomm, valr, resvalr, MPI_PROD)

    if (mycomm%lead) then
      if (mycomm%size <= max_valid_rank + 1) then
        max_none_one_rank = mycomm%size
      else
        max_none_one_rank = max_valid_rank + 1
      end if

      @:ASSERT(abs(resvalr(1) - (gamma(real(max_none_one_rank + 1, kind=dp)) * (1.2)**(max_none_one_rank))) < abs((resvalr(1)*1e-6)))
      @:ASSERT(abs(resvalr(2) - (gamma(real(max_none_one_rank + 1, kind=dp)) * (4.3)**(max_none_one_rank))) < abs((resvalr(2)*1e-6)))
      @:ASSERT(abs(resvalr(3) - (gamma(real(max_none_one_rank + 1, kind=dp)) * (3.8)**(max_none_one_rank))) < abs((resvalr(3)*1e-6)))
    else
      @:ASSERT(resvalr(1) == 0.0_dp)
      @:ASSERT(resvalr(2) == 0.0_dp)
      @:ASSERT(resvalr(3) == 0.0_dp)
    end if
  $:END_TEST()

  $:TEST("ip_sum")
    integer, parameter :: dp = kind(1.0d0)
    type(mpifx_comm) :: mycomm
    real(dp) :: resvalr(3)

    call mycomm%init(global_comm_id())
    resvalr(:) = [ real(mycomm%rank + 1, dp) * 1.2, &
        & real(mycomm%rank + 1, dp) * 4.3, real(mycomm%rank + 1, dp) * 3.8 ]
    call mpifx_reduceip(mycomm, resvalr, MPI_SUM)

    if (mycomm%lead) then
      @:ASSERT(abs(resvalr(1) -  (mycomm%size * (mycomm%size + 1)) / 2 * 1.2) < abs((resvalr(1)*1e-7)))
      @:ASSERT(abs(resvalr(2) -  (mycomm%size * (mycomm%size + 1)) / 2 * 4.3) < abs((resvalr(2)*1e-7)))
      @:ASSERT(abs(resvalr(3) -  (mycomm%size * (mycomm%size + 1)) / 2 * 3.8) < abs((resvalr(3)*1e-7)))
    else
      @:ASSERT(resvalr(1) == real(mycomm%rank + 1, dp) * 1.2)
      @:ASSERT(resvalr(2) == real(mycomm%rank + 1, dp) * 4.3)
      @:ASSERT(resvalr(3) == real(mycomm%rank + 1, dp) * 3.8)
    end if
  $:END_TEST()

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("reduce", test_list([&
            $:TEST_ITEMS()
        ]))&
    ])
    @:STOP_ON_MISSING_TEST_ITEMS()

  end function tests

end module test_reduce
