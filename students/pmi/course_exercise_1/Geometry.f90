include 'kinds.f90'

module Geometry
use kinds
implicit none

type :: vector3d
  real(dp) :: x, y, z
end type

type :: point3d
  real(dp) :: x, y, z
end type

interface operator(+)
  module procedure sumvv, sumvp, sumpv
end interface

interface operator(/)
  module procedure div_vr
end interface

interface operator(-)
  module procedure minusvv, subpv, subpp
end interface

interface operator(.dot.)
  module procedure dot_prod
end interface

interface operator(.cross.)
  module procedure cross_prod
end interface

interface operator(*)
  module procedure mulrv, mulvr
end interface




contains
!--------------------------------------------------------------------------------------------------------------
!
!            Functions for operating vectors and points

  !
  !    Division
  !

  elemental type(vector3d) function div_vr(b, a) ! Vector b / real a = (b1/a, b2/a, b3/a)
    real(dp), intent(in) :: a
    type(vector3d), intent(in) :: b
    div_vr = vector3d(b%x/a, b%y/a, b%z/a)
  end function
  
  !
  !    Sum
  !
  
  elemental type(vector3d) function sumvv(a, b)
    type(vector3d), intent(in) :: a, b
    sumvv = vector3d(a%x + b%x, a%y + b%y, a%z + b%z)
  end function
  
  elemental type(point3d) function sumvp(a, b)
    type(vector3d), intent(in) :: a
    type(point3d), intent(in) :: b
    sumvp = point3d(a%x + b%x, a%y + b%y, a%z + b%z)
  end function
  
  elemental type(point3d) function sumpv(b, a)
    type(vector3d), intent(in) :: a
    type(point3d), intent(in) :: b
    sumpv = point3d(a%x + b%x, a%y + b%y, a%z + b%z)
  end function
  
  !
  !    Subtraction
  !
  
  elemental type(vector3d) function minusvv(a,b)
    type(vector3d), intent(in) :: a, b
    minusvv = vector3d(a%x - b%x, a%y - b%y, a%z - b%z)
  end function
  
  elemental type(vector3d) function subpp(a,b)
    type(point3d), intent(in) :: a, b
    subpp = vector3d(a%x - b%x, a%y - b%y, a%z - b%z)
  end function
  
  elemental type(point3d) function subpv(a, b)
    type(point3d), intent(in) :: a
    type(vector3d), intent(in) :: b
    subpv = point3d(a%x - b%x, a%y - b%y, a%z - b%z)
  end function
  
  elemental real(dp) function dot_prod(a,b)
    type(vector3d), intent(in) :: a, b
    dot_prod = a%x * b%x + a%y * b%y + a%z * b%z
  end function
  
  elemental type(vector3d) function cross_prod(a,b) ! 
    type(vector3d), intent(in) :: a, b
    cross_prod = vector3d(a%y*b%z - a%z*b%y, a%z*b%x - a%x*b%z, a%x*b%y - a%y*b%x)
  end function
  
  elemental type(vector3d) function mulrv(a, b)
    real(dp), intent(in) :: a
    type(vector3d), intent(in) :: b
    mulrv = vector3d(a*b%x, a*b%y, a*b%z)
  end function
  
  elemental type(vector3d) function mulvr(b, a)
    real(dp), intent(in) :: a
    type(vector3d), intent(in) :: b
    mulvr = vector3d(a*b%x, a*b%y, a*b%z)
  end function
  
  real(dp) function distance(a,b)
    type(vector3d), intent(in) :: a, b
    type(vector3d) :: c
    c = a - b
    distance = sqrt( c.dot.c )
  end function


end module
