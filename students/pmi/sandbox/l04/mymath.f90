include 'kinds.f90'

module mymath
  use kinds
  implicit none


  real(dp), parameter :: pi = 3.14159

contains
  real(dp) function circle_area(r)
    real(dp), intent(in) :: r
    circle_area = pi * r**2
  end function


end module
