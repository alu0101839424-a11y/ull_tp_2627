include 'mymath.f90'
!~ include 'kinds.f90' ! Not necesary here because mymath already uses it

program test_mymath
use mymath
!~ use kinds ! Not necesary here because mymath already uses it

implicit none

real(dp) :: r, area

r = 2.0


area = circle_area(r)
print *, area


end program
