include 'Geometry.f90'

program Operators

use Geometry
implicit none

type(vector3d) :: a, b, c
type(vector3d), dimension(10) :: biga, bigb

a = vector3d(1,1,1)
b = vector3d(1,0,-1)

print*, a.dot.b
print*, a.cross.a, a.cross.b

c = a.cross.b

print*, c.dot.a, c.dot.b

! Both of the following commands give the same error. They are due to the fact
! that, since we didn't give an order for this operators, they act from left to right.

! Thus, first operation tries to multiply 2 * a first, which is not defined here;
! and the second one tries to sum 1 + a firstly, also a non-defined operation here

!~ print*, 2.0 * a.dot.b
!~ print*, 1.0 + a .dot. b

! We can actually try making the first one work by uncomenting the real x vector patches on the module


end program
