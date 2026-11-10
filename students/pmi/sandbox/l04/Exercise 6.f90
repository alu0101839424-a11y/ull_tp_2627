include 'photometry.f90'

program test_mymath
use photometry

implicit none

real :: F1(1), F2(20), m1(1), m2(20)
integer :: i

F1 = 10
do i = 1,20
  F2(i) = i
end do

m1 = flux_to_mag(F1)
m2 = flux_to_mag(F2)

!~ print *, m1
!~ print *, m2


end program
