module stats
  implicit none
  private
  public :: mean_and_std


contains
  subroutine mean_and_std(x, mean, std)
    real, intent(in), allocatable :: x(:)
    real, intent(out) :: mean, std
    integer :: n
    
    !~     x = 2*x - 3  ! Of course, since we told the code x is 'in', it does not want to change it, giving an error
    
    n = size(x)
    mean = sum(x) / n
    std = sqrt( sum((x - mean)**2) / n )
  end subroutine


end module
