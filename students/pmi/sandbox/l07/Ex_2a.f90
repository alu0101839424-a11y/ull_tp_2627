program Table

implicit none

integer, parameter :: n = 8
integer :: i
character(len=*), parameter :: archivo = 'Tabla.dat'
character(len=8) :: names(n)
real :: a(n), P
character(len=:), allocatable :: formato, formato2

formato = '(A10 "     |", F12.3 "      |", F15.3 "      |", F15.1)'
formato2 = '(A10, F12.3, F15.3, F15.1)'



names = [ 'Mercury ', 'Venus   ', 'Earth   ', 'Mars    ', 'Jupiter ', 'Saturn  ', 'Uranus  ', 'Neptune ']
a = [0.387, 0.723, 1.000, 1.524, 5.203, 9.537, 19.191, 30.069]


open(1, file = archivo)
write(1,'(A10 "     |", " "A12 "     |", " "A15 "     |", " "A15)') 'Planets ', 'a (AU)', 'P (years)', 'P (days)'
write(1,*) '--------------------------------------------------------------------------'
do i = 1, n
  P = a(i)**1.5
  write(1, formato) names(i), a(i), P, P*365.25
end do

close(1)


!    We will do the same, but without |, so that we can read them for the next part

open(1, file = 'Tabla_legible.dat')
write(1,'(A10 ,A12, A15,A15)') 'Planets ', 'a (AU)', 'P (years)', 'P (days)'
write(1,*) '--------------------------------------------------------------------------'
do i = 1, n
  P = a(i)**1.5
  write(1, formato2) names(i), a(i), P, P*365.25
end do

close(1)


write(*,*) 'Program executed propperly'

end program
