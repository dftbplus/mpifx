program testapp
  use fortuno_mpi, only : execute_mpi_cmd_app, test_list
  use test_allgather, only : allgather_tests => tests
  use test_allgatherv, only : allgatherv_tests => tests
  use test_allreduce, only : allreduce_tests => tests
  use test_bcast, only : bcast_tests => tests
  use test_comm_split_type, only : comm_split_type_tests => tests
  use test_comm_split, only : comm_split_tests => tests
  use test_gather, only : gather_tests => tests
  use test_gatherv, only : gatherv_tests => tests
  use test_reduce, only : reduce_tests => tests
  use test_scatter, only : scatter_tests => tests
  use test_scatterv, only : scatterv_tests => tests
  use test_send_recv, only : send_recv_tests => tests
  implicit none

  call execute_mpi_cmd_app(
      test_list([&
          allgather_tests(),&
          allgatherv_tests(),&
          allreduce_tests(),&
          bcast_tests(),&
          comm_split_type_tests(),&
          comm_split_testS(),&
          gather_tests(),&
          gatherv_tests(),&
          reduce_tests(),&
          scatter_tests(),&
          scatterv_tests(),&
          send_recv_tests()&
      ]&
  )

end program testapp
