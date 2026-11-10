! Two ranks swap arrays of n numbers. Both send first and then receive,
! which works for small messages and deadlocks for large ones.
! Run with two ranks: mpirun -np 2 ./exchange 100000
program exchange
  use mpi_f08
  use iso_fortran_env, only: dp => real64
  implicit none
  integer :: rank, other, n
  character(len=32) :: arg
  real(dp), allocatable :: a(:), b(:)

  call mpi_init()
  call mpi_comm_rank(MPI_COMM_WORLD, rank)
  other = 1 - rank

  call get_command_argument(1, arg)
  read(arg, *) n
  allocate(a(n), b(n))
  a = real(rank, dp)

  call mpi_send(a, n, MPI_DOUBLE_PRECISION, other, 0, &
                MPI_COMM_WORLD)
  call mpi_recv(b, n, MPI_DOUBLE_PRECISION, other, 0, &
                MPI_COMM_WORLD, MPI_STATUS_IGNORE)
  print *, 'Rank', rank, 'received', b(1)

  call mpi_finalize()
end program exchange
