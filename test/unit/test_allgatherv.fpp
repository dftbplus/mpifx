!> Test various patterns of allgatherv
#:include "fortuno_mpi.fypp"

module test_allgatherv
  use libmpifx_module, only : mpifx_comm, mpifx_allgatherv
  use fortuno_mpi, only : global_comm_id, suite => mpi_suite_item, test_list, is_close, is_equal
  $:FORTUNO_MPI_IMPORTS()
  implicit none

contains

  $:TEST("R1_to_R1")
    type(mpifx_comm) :: mycomm
    integer, parameter :: sp = kind(1.0)
    real(sp), allocatable :: send1(:)
    real(sp), allocatable :: recv1(:)
    integer, allocatable :: recvcounts(:)
    integer :: ii, nrecv

    call mycomm%init(global_comm_id())
    allocate(send1(mycomm%rank+1), source = 0.0_sp)
    send1 = real(mycomm%rank+1, sp)
    ! recv1 size is 1+2+3+...+mycomm%size
    nrecv = mycomm%size*(mycomm%size+1)/2
    allocate(recv1(nrecv), source = 0.0_sp)
    allocate(recvcounts(mycomm%size), source = 0)
    do ii = 1, mycomm%size
      recvcounts(ii) = ii
    end do
    call mpifx_allgatherv(mycomm, send1, recv1, recvcounts)

    @:ASSERT(is_close(sum(recv1), (2*mycomm%size**3+3*mycomm%size**2+mycomm%size)/6.0_sp, &
        & atol=epsilon(1.0_sp), rtol=0.0_sp))
  $:END_TEST()

  $:TEST("R2_to_R2")
    type(mpifx_comm) :: mycomm
    integer, parameter :: sp = kind(1.0)
    real(sp), allocatable :: send2(:,:)
    real(sp), allocatable :: recv2(:,:)
    integer, allocatable :: recvcounts(:)
    integer :: ii, nrecv, nCol

    call mycomm%init(global_comm_id())
    nCol = 5
    allocate(send2(nCol, mycomm%rank+1), source = 0.0_sp)
    send2 = real(mycomm%rank + 1, sp)
    nrecv = mycomm%size*(mycomm%size+1)/2
    allocate(recv2(nCol, nrecv), source = 0.0_sp)
    allocate(recvcounts(mycomm%size), source = 0)
    do ii = 1, mycomm%size
      recvcounts(ii) = nCol*ii
    end do
    call mpifx_allgatherv(mycomm, send2, recv2, recvcounts)

    @:ASSERT(is_close(sum(recv2), nCol*mycomm%size*(mycomm%size+1)*(2*mycomm%size+1)/6.0_sp, &
        & atol=epsilon(1.0_sp), rtol=0.0_sp))
  $:END_TEST()

  $:TEST("R0_to_R1")
    ! R0 -> R1 with specified receive pattern
    type(mpifx_comm) :: mycomm
    integer, parameter :: sp = kind(1.0)
    real(sp), allocatable :: recv1(:)
    real(sp) :: send0
    integer, allocatable :: recvcounts(:)
    integer, allocatable :: displs(:)
    integer :: ii, nrecv

    call mycomm%init(global_comm_id())
    send0 = real(mycomm%rank + 1, sp)
    nrecv = mycomm%size
    allocate(recv1(nrecv), source = 0.0_sp)
    allocate(recvcounts(mycomm%size), source = 1)
    allocate(displs(mycomm%size), source = 0)
    ! set a non trivial displs vector
    do ii = 1, mycomm%size
      displs(ii) = mycomm%size - ii
    end do
    call mpifx_allgatherv(mycomm, send0, recv1, recvcounts, displs)

    @:ASSERT(is_close(sum(recv1), (mycomm%size*(mycomm%size+1))/2.0_sp, &
        & atol=epsilon(1.0_sp), rtol=0.0_sp))
  $:END_TEST()

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("allgatherv", test_list([&
            $:TEST_ITEMS()
        ]))&
    ])
    @:STOP_ON_MISSING_TEST_ITEMS()

  end function tests

end module test_allgatherv
