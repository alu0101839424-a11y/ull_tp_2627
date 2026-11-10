module Geometry
implicit none

type :: vector3d
  real :: x, y, z
end type

interface operator(+)
  module procedure sumvv
end interface

interface operator(/)
  module procedure div_vr
end interface

interface operator(-)
  module procedure minusvv
end interface

interface operator(.dot.)
  module procedure dot_prod
end interface

interface operator(.cross.)
  module procedure cross_prod
end interface



!      For real x vector

!~ interface operator(*)
!~ module procedure mulrv
!~ end interface




contains
!--------------------------------------------------------------------------------------------------------------
!
!            Functions for operating vectors

  elemental type(vector3d) function div_vr(b, a) ! Vector b / real a: (b1/a, b2/a, b3/a)
    real, intent(in) :: a
    type(vector3d), intent(in) :: b
    div_vr = vector3d(b%x/a, b%y/a, b%z/a)
  end function
  
  elemental type(vector3d) function sumvv(a, b)
    type(vector3d), intent(in) :: a, b
    sumvv = vector3d(a%x + b%x, a%y + b%y, a%z + b%z)
  end function
  
  elemental type(vector3d) function minusvv(a,b)
    type(vector3d), intent(in) :: a, b
    minusvv = vector3d(a%x - b%x, a%y - b%y, a%z - b%z)
  end function
  
  elemental real function dot_prod(a,b)
    type(vector3d), intent(in) :: a, b
    dot_prod = a%x * b%x + a%y * b%y + a%z * b%z
  end function
  
  elemental type(vector3d) function cross_prod(a,b) ! 
    type(vector3d), intent(in) :: a, b
    cross_prod = vector3d(a%y*b%z - a%z*b%y, a%z*b%x - a%x*b%z, a%x*b%y - a%y*b%x)
  end function
  
!---------------------------------------------------------------------------------------------------------------  


!    For real x vector

!~ pure type(vector3d) function mulrv(a, b)
!~ real, intent(in) :: a
!~ type(vector3d), intent(in) :: b
!~ mulrv = vector3d(a*b%x, a*b%y, a*b%z)
!~ end function mulrv


end module
