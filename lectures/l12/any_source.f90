program any_source
  use mpi_f08
  implicit none
  integer :: rank, n_ranks, i, x
  type(MPI_Status) :: status

  call mpi_init()
  call mpi_comm_rank(MPI_COMM_WORLD, rank)
  call mpi_comm_size(MPI_COMM_WORLD, n_ranks)

  if (rank == 0) then
    do i = 1, n_ranks - 1
      call mpi_recv(x, 1, MPI_INTEGER, MPI_ANY_SOURCE, &
                    MPI_ANY_TAG, MPI_COMM_WORLD, status)
      print *, 'Got', x, 'from rank', status%MPI_SOURCE
    end do
  else
    x = rank**2
    call mpi_send(x, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD)
  end if

  call mpi_finalize()
end program any_source
