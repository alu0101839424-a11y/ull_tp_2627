module photometry
  implicit none
  
contains
  function flux_to_mag(flux) result(m)
    real, intent(in) :: flux(:)
    real, allocatable :: m(:)
    integer :: N
    
    N = size(flux)
    allocate(m(N))
    
    where (flux > 0.0)
      m = -2.5 * log10(flux)
    elsewhere
      m = 0
    end where
    print *, m  ! don't see the problem, my compiler allows this and it runs fine
  end function


end module
