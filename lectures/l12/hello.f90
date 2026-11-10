program hello
  use mpi_f08
  implicit none
  integer :: rank, n_ranks

  call mpi_init()
  call mpi_comm_size(MPI_COMM_WORLD, n_ranks)
  call mpi_comm_rank(MPI_COMM_WORLD, rank)

  print *, 'Hello from rank', rank, 'of', n_ranks

  call mpi_finalize()
end program hello
