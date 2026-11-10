include 'stats.f90'

program using_stats
use stats, only: mean_and_std
implicit none

real, allocatable :: x(:)
real :: mu, sigma

x = [4.2, 3.9, 4.5, 4.1, 4.3]

call mean_and_std(x, mu, sigma)
print *, 'Mean:',mu
print *, 'Std:', sigma

end program
