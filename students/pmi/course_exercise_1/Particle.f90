include 'Geometry.f90'

module Particle
use Geometry
implicit none

type :: particle3d
  type(point3d) :: p
  type(vector3d) :: v
  real(dp) :: m
end type

end module
