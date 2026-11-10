module Ex1_module
implicit none

type :: vector3d
  real :: x, y, z
end type

type :: star
  character(len = :), allocatable :: name
  real :: mass
  type(vector3d) :: position
end type

interface operator(+)
  module procedure sumvv
end interface
interface operator(/)
  module procedure div_vr
end interface

contains
!--------------------------------------------------------------------------------------------------------------
!
!            Functions for operating vectors

  pure type(vector3d) function div_vr(b, a) ! Vector b / real a: (b1/a, b2/a, b3/a)
    real, intent(in) :: a
    type(vector3d), intent(in) :: b
    div_vr = vector3d(b%x/a, b%y/a, b%z/a)
  end function
  
  pure type(vector3d) function sumvv(a, b)
    type(vector3d), intent(in) :: a, b
    sumvv = vector3d(a%x + b%x, a%y + b%y, a%z + b%z)
  end function
  
!---------------------------------------------------------------------------------------------------------------  
  
  function centre_of_mass(input_stars) result(center)
    type(star), dimension(:), intent(in) :: input_stars
    type(vector3d) :: center
    integer :: N_stars, i
    
    N_stars = size(input_stars)
    center = vector3d(0,0,0)
    do i = 1, N_stars
      center = center + input_stars(i)%position
    end do
    center = center / real(N_stars)
  end function

end module
