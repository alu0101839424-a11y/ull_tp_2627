program bcast
  use mpi_f08
  implicit none
  integer :: rank, n

  call mpi_init()
  call mpi_comm_rank(MPI_COMM_WORLD, rank)

  if (rank == 0) read *, n    ! only rank 0 reads
  call mpi_bcast(n, 1, MPI_INTEGER, 0, MPI_COMM_WORLD)
  print *, 'Rank', rank, 'has n =', n

  call mpi_finalize()
end program bcast
