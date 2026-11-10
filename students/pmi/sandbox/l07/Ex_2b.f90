program Table2  !    Not finished!!!!

implicit none

integer, parameter :: n = 8
integer :: i
character(len=*), parameter :: archivo = 'Tabla.dat'
character(len=8) :: names(n)
real :: a(n), P(n), P_mean


open(1, file = 'Tabla_legible.dat', action = 'read')

read(1,*)
read(1,*)

do i = 1, n
  read(1,*) names(i), a(i), P(i)
end do

close(1)

P_mean = sum(P) / n

write(*,*) 'Mean period is:', P_mean
write(*,*) 'Program run propperly'

end program
