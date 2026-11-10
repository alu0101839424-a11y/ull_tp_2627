program Table

implicit none

integer, parameter :: n = 8
integer :: i
character(len=8) :: names(n)
real :: a(n), P
character(len=:), allocatable :: formato, formato2, formato3

formato = '(A10 "     |", F12.3 "      |", F15.3 "      |", F15.1)'
formato2 = '(A10, "     |", F20.2, "     |", ES20.5, "     |", G0)'
formato3 = '(A10, "     |", F10.2, "     |", ES10.5, "     |", G0)'




names = [ 'Mercury ', 'Venus   ', 'Earth   ', 'Mars    ', 'Jupiter ', 'Saturn  ', 'Uranus  ', 'Neptune ']
a = [0.387, 0.723, 1.000, 1.524, 5.203, 9.537, 19.191, 30.069]

print'(A10 "     |", " "A12 "     |", " "A15 "     |", " "A15)', 'Planets ', 'a (AU)', 'P (years)', 'P (days)'
print*, '--------------------------------------------------------------------------'

do i = 1, n
  P = a(i)**1.5
  print formato, names(i), a(i), P, P*365.25
end do

print*, ''
print*, ''
print*, '--------------------------------------------------------------------------'
print*, ''

print '(A10, "     |", A20, "     |", A20, "     |", A20)', &
    'Planets', 'F format (s)', 'ES format (s)', 'G0 format (s)'

print *, '--------------------------------------------------------------------------------------------'

do i = 1, n
    P = a(i)**1.5 * 365.25*24*3600
    print formato2, names(i), P, P, P
end do

!   I guess the best option is es, since it is the only one using
!     scientific notation, which helps with these big numbers.

print*, ''
print*, ''
print *, '--------------------------------------------------------------------------------------------'
print*, ''
print*, ''
print*, 'Now narrower'

do i = 1, n
    P = a(i)**1.5 * 365.25*24*3600
    print formato3, names(i), P, P, P
end do

!    The numbers that don't fit in their respective space become '*************'
!   This happens because the program doesn't know how to tell the number with the conditions asked.


print*, ''
print*, ''
print *, '--------------------------------------------------------------------------------------------'
print*, ''
print*, ''

print '(A10, *("|"es10.2"  |"))', 'Periods: ', a**1.5

end program
