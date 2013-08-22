include(mpifx_common.m4)

dnl ************************************************************************
dnl *** mpifx_bcast
dnl ************************************************************************

define(`_subroutine_mpifx_bcast',`dnl
dnl $1: subroutine suffix
dnl $2: dummy arguments type
dnl $3: dummy arguments rank specifier ("", (:), (:,:), etc.)
dnl $4: dummy arguments size (1 or size(dummyname))
dnl $5: corresponding MPI type
!> Broadcasts an MPI message to all nodes (type $1).
!! \param mycomm MPI descriptor
!! \param msg  Msg to be broadcasted on root and received on non-root
!!     nodes.
!! \param root  Root node for the broadcast (default: mycomm%masterrank).
!! \param error  Optional error handling flag.
!!
subroutine mpifx_bcast_$1(mycomm, msg, root, error)
  type(mpifx_comm), intent(in) :: mycomm
  $2 :: msg$3
  integer, intent(in), optional :: root
  integer, intent(out), optional :: error

  integer :: root0, error0

  _handle_inoptflag(root0, root, mycomm%masterrank)
  call mpi_bcast(msg, $4, $5, root0, mycomm%id, error0)
  call handle_errorflag(error0, "MPI_BCAST in mpifx_bcast_$1", error)
    
end subroutine mpifx_bcast_$1
')
