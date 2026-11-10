program trapezoid
  use mpi_f08
  use iso_fortran_env, only: dp => real64
  implicit none
  integer, parameter :: n = 1000000
  real(dp), parameter :: a = 0.0_dp, b = 1.0_dp
  integer :: rank, n_ranks, i, i1, i2
  real(dp) :: h, local, total

  call mpi_init()
  call mpi_comm_rank(MPI_COMM_WORLD, rank)
  call mpi_comm_size(MPI_COMM_WORLD, n_ranks)

  ! This rank's block of the inner points
  h = (b - a) / n
  i1 = rank * (n - 1) / n_ranks + 1
  i2 = (rank + 1) * (n - 1) / n_ranks

  local = 0.0_dp
  do i = i1, i2
    local = local + f(a + i * h)
  end do

  ! Add the end points only once in total
  if (rank == 0) then
    local = local + 0.5_dp * (f(a) + f(b))
  end if

  call mpi_reduce(local, total, 1, &
                  MPI_DOUBLE_PRECISION, MPI_SUM, &
                  0, MPI_COMM_WORLD)
  if (rank == 0) print *, 'Integral:', h * total

  call mpi_finalize()

contains

  pure real(dp) function f(x)
    real(dp), intent(in) :: x
    f = 4.0_dp / (1.0_dp + x**2)
  end function f

end program trapezoid
