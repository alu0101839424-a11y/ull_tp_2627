program order_of_sum
implicit none


integer, parameter :: N = 10**7
real :: s1, s2
integer :: k

!-----------------------------------------------------------------
!
!    Double precission definitions
!    -----------------------------
!-----------------------------------------------------------------
integer, parameter :: ik = selected_int_kind(15)
integer, parameter :: dp = selected_real_kind(15, 307)
integer(ik), parameter :: Nd = 10**7
integer(ik) :: kd
real(dp) :: s3, s4
!-----------------------------------------------------------------

s1 = 0
do k = 1, N
  s1 = s1 + 1.0/real(k)
end do

s2 = 0
do k = N, 1, -1
  s2 = s2 + 1.0/real(k)
end do

s3 = 0
do kd = 1, Nd
  s3 = s3 + 1.0/real(kd)
end do

s4 = 0
do kd = Nd, 1, -1
  s4 = s4 + 1.0/real(kd)
end do

print*, s1,s2,s3,s4

write(*,*) 'The last two sums (with double precission) only differ in the last two digits.'
write(*,*) 'This means they are probably quite close to the real value of the sum'
write(*,*) ''
write(*,*) 'On the other hand, between the first two (with single precission), the second one'
write(*,*) 'is clearly the closest to the real value.'
write(*,*) ''
write(*,*) 'I suspect this is due to the fact that, when k reaches N, 1/k is too small compared'
write(*,*) 'to the cumulative sum, which should be already close to 15. Thus, the first terms'
write(*,*) 'of the sum take the sum to a high order (arround 10), which makes the last terms be'
write(*,*) 'out of the scope of the simple precission calculations.'
write(*,*) ''
write(*,*) 'On the other hand, if we start with k = N, we start by adding the least noticeable'
write(*,*) 'numbers first, which will be more precise since they are of the same order; because'
write(*,*) 'they will be truncated at a certain number of decimals, but with the exponent of their order.'

end program
