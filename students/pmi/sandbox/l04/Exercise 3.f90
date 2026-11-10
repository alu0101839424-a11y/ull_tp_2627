program quadratic_eq
implicit none


real :: a, b, c, x_p, x_m

!-----------------------------------------------------------------
!
!    Double precision definitions
!    -----------------------------
!-----------------------------------------------------------------
integer, parameter :: dp = selected_real_kind(15, 307)
real(dp) :: a2, b2, c2, x_p2, x_m2
!-----------------------------------------------------------------

a = 1
b = -1e4
c = 1

x_p = (-b + sqrt(b**2 - 4*a*c))/(2*a)
x_m = (-b - sqrt(b**2 - 4*a*c))/(2*a)
print*, 'x_p =', x_p,'x_m =', x_m
print*, ''
print*, 'This is due to the precision. We can check that the smallest number'
print*, 'that can be added to b^2 without being unnoticed by Fortran is:'
print*, b**2*epsilon(b**2)
print*, 'Which is bigger than 4ac = 4. One thing we could do to solve this'
print*, 'without using more precision is to use the relation: x_p*x_m = c/a'
print*, '==> x_m = (c/a) / x_p'

x_m = c/a/x_p
print*, 'With this, the two solutions are approximately:', x_p, 'and', x_m

!------------------------------------------------------------------------------------
!
!         With double precision
!
!------------------------------------------------------------------------------------

a2 = 1
b2 = -1e4
c2 = 1

x_p2 = (-b2 + sqrt(b2**2 - 4*a2*c2))/(2*a2)
x_m2 = (-b2 - sqrt(b2**2 - 4*a2*c2))/(2*a2)
print*, ''
print*, 'With double precision'
print*, ''
print*, 'x_p =', x_p2,'x_m =', x_m2



end program
