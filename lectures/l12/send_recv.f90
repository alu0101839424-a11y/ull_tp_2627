program send_recv
  use mpi_f08
  use iso_fortran_env, only: dp => real64
  implicit none
  integer :: rank
  real(dp) :: x(4)

  call mpi_init()
  call mpi_comm_rank(MPI_COMM_WORLD, rank)

  if (rank == 0) then
    x = [1.0_dp, 4.0_dp, 9.0_dp, 16.0_dp]
    call mpi_send(x, 4, MPI_DOUBLE_PRECISION, 1, 42, &
                  MPI_COMM_WORLD)
  else if (rank == 1) then
    call mpi_recv(x, 4, MPI_DOUBLE_PRECISION, 0, 42, &
                  MPI_COMM_WORLD, MPI_STATUS_IGNORE)
    print *, 'Rank 1 received', x
  end if

  call mpi_finalize()
end program send_recv
