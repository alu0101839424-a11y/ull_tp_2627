program mach_epsilon
implicit none

real :: num_s, eps1

!-----------------------------------------------------------------
!
!    Double precission definitions
!    -----------------------------
!-----------------------------------------------------------------
integer, parameter :: dp = selected_real_kind(15, 307)
real(dp) :: num_d, eps2
!-----------------------------------------------------------------

num_s = 2
eps1 = 1
do while (num_s>1.0)
  eps1 = eps1 / 2
  num_s = 1.0 + eps1
end do

print*, 'Obtained epsilon:             ', 2*eps1
print*, 'Given by the epsilon function:',epsilon(num_s)

num_d = 2
eps2 = 1
do while (num_d>1.0)
  eps2 = eps2 / 2
  num_d = 1.0 + eps2
end do

print*, ''
print*, 'Obtained epsilon:             ', 2*eps2
print*, 'Given by the epsilon function:',epsilon(num_d)

end program
