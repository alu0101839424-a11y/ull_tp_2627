program growing_array  !       Not finished!!!
implicit none

real :: x
real, allocatable :: a(:)
integer :: ierr, i, n

write(*,*) 'Write your numbers, one by one.'
write(*,*) 'Once you have finished, input another type of variable'
allocate(a(0))
do
  read(*,*, iostat=ierr) x
    if (ierr /= 0) then
      write(*,*) "Finish input detected. Array will be closed."
      exit
    end if
    a = [a,x]
end do

n = size(a)
print*, 'a                   mean               reverse array'
print*, '                ',sum(a)/n
do i = 1, n
  print*, a(i), '                   ', a(n-i+1)
end do

end program
