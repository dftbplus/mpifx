program testapp
  use fortuno_mpi, only : execute_mpi_cmd_app
  use test_bcast, only : bcast_test_items
  implicit none

  call execute_mpi_cmd_app(&
    testitems=[&
      bcast_test_items()&
    ]&
  )

end program testapp
