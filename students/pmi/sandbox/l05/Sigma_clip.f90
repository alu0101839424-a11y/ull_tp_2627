include 'stats.f90'

module sigma_clip

use stats
implicit none

  contains

function clip(x,nsigma) result(solution)
  real :: nsigma, mu, sigma
  real, allocatable :: x(:), solution(:)
  integer :: N, i
  logical, allocatable :: inside(:)


  N = size(x)

  call mean_and_std(x, mu, sigma)

  inside = ((x>(mu-nsigma * sigma)) .and. (x<(mu+nsigma * sigma)))

  allocate(solution(0))
  do i = 1, N
    if (inside(i)) then
      solution = [solution,x(i)]
    end if
  end do

end function

end module
