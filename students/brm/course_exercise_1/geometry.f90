! This geometry module defines 3D point and vector types and their corresponding arithmetic operations to be later used more easily.

module geometry
  use kinds, only: dp
  implicit none
  private
  
  ! We will make only some variables available outside the module, just in case
  public :: vector3d, point3d, distance
  public :: operator(+), operator(-), operator(*), operator(/)

  !we define the vector and point types as we did on lesson 6
  type :: vector3d
    real(dp) :: x, y, z
  end type vector3d

  type :: point3d
    real(dp) :: x, y, z
  end type point3d

  ! here we define the interface operators and which functions they will use
  interface operator(+)
    module procedure sumvp
    module procedure sumpv
  end interface

  interface operator(-)
    module procedure subpv
    module procedure subpp
  end interface

  interface operator(*)
    module procedure mulrv
    module procedure mulvr
  end interface

  interface operator(/)
    module procedure divvr
  end interface

contains

  ! Adds a vector to a point, resulting in a new point
  pure function sumvp(v, p) result(res)
    type(vector3d), intent(in) :: v
    type(point3d), intent(in) :: p
    type(point3d) :: res
    res%x = v%x + p%x
    res%y = v%y + p%y
    res%z = v%z + p%z
  end function sumvp

  ! Adds a point to a vector, resulting in a new point
  pure function sumpv(p, v) result(res)
    type(point3d), intent(in) :: p
    type(vector3d), intent(in) :: v
    type(point3d) :: res
    res%x = p%x + v%x
    res%y = p%y + v%y
    res%z = p%z + v%z
  end function sumpv

  ! Subtracts a vector from a point, resulting in a new point
  pure function subpv(p, v) result(res)
    type(point3d), intent(in) :: p
    type(vector3d), intent(in) :: v
    type(point3d) :: res
    res%x = p%x - v%x
    res%y = p%y - v%y
    res%z = p%z - v%z
  end function subpv

  ! Subtracts a point from a point, resulting in a vector displacement
  pure function subpp(p1, p2) result(res)
    type(point3d), intent(in) :: p1, p2
    type(vector3d) :: res
    res%x = p1%x - p2%x
    res%y = p1%y - p2%y
    res%z = p1%z - p2%z
  end function subpp

  ! Multiplies a real scalar by a vector, resulting in a scaled vector
  pure function mulrv(r, v) result(res)
    real(dp), intent(in) :: r
    type(vector3d), intent(in) :: v
    type(vector3d) :: res
    res%x = r * v%x
    res%y = r * v%y
    res%z = r * v%z
  end function mulrv

  ! Multiplies a vector by a real scalar, resulting in a scaled vector
  pure function mulvr(v, r) result(res)
    type(vector3d), intent(in) :: v
    real(dp), intent(in) :: r
    type(vector3d) :: res
    res%x = v%x * r
    res%y = v%y * r
    res%z = v%z * r
  end function mulvr

  ! Divides a vector by a real scalar, resulting in a scaled vector
  pure function divvr(v, r) result(res)
    type(vector3d), intent(in) :: v
    real(dp), intent(in) :: r
    type(vector3d) :: res
    res%x = v%x / r
    res%y = v%y / r
    res%z = v%z / r
  end function divvr

  ! we define the function that calculates the Euclidean distance between two points
  pure function distance(p1, p2) result(res)
    type(point3d), intent(in) :: p1, p2
    real(dp) :: res
    res = sqrt((p1%x - p2%x)**2 + (p1%y - p2%y)**2 + (p1%z - p2%z)**2)
  end function distance
 ! we have been using pure functions since they will only use input arguments. Also, this makes the compiler able to optimize it better. Also,
  ! in the future, multiple OpenMP threads may execute them concurrently without risking memory collisions
end module geometry