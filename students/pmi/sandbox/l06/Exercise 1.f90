include 'Ex1 module.f90'

program types

use Ex1_module
implicit none

type(star), dimension(3) :: stars
type(star) :: one


stars(1) = star('Sun', 1, vector3d(0,0,0))
stars(2) = star('Me', 1e-7, vector3d(1/sqrt(2.0),1/sqrt(2.0),0))
stars(3) = star('My cat', 3, vector3d(1,2,3))

print*, 'Our stars are: ', stars(1)%name, ', ', stars(2)%name, ' and ', stars(3)%name
print*, 'Their masses are:', stars%mass

one = stars(1)
!~ print*, one Not allowed for name being allocatable, apparently

print*, centre_of_mass(stars)

end program
