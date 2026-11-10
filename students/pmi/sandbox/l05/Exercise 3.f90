!~ include 'stats.f90'
include 'Sigma_clip.f90'

program sigma_clipping

use sigma_clip
implicit none

real :: nsigma, mu, sigma
real, allocatable :: x(:), clipped(:)

x = [10.1, 9.8, 10.3, 9.9, 55.0, 10.0, 10.2]
nsigma = 2

call mean_and_std(x, mu, sigma)
print*, mu, sigma

clipped = clip(x, nsigma)

print *, x
print *, clipped     ! Of course, it only leaves 55 out

! We don't need to allocate clipped, since the program already realizes
! it has to be of the same dimension as the result of the function

x = [10.1, 9.8, 10.3, 9.9, 55.0, 10.0, 10.2]
do while (size(x) /= size(clipped))
  x = clipped
  clipped = clip(x, nsigma)
end do

call mean_and_std(x, mu, sigma)
print*, x
print*, mu, sigma

!  No more values are removed, since the new mean is 10.05, and sigma > 0.15 and all current values are in mu +- 2sigma

end program
