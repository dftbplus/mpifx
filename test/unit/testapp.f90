program testapp
  use fortuno_mpi, only : execute_mpi_cmd_app, test_list
  use test_bcast, only : bcast_tests => tests
  implicit none

  call execute_mpi_cmd_app(test_list([&
      bcast_tests()&
  ]))

end program testapp
