!  I think I did this a little differently from what is asked
!
! Rereading now, I think what I had to do is creating an array of size
! maxval(a) (just doing allocate(counts(maxval(a)))), so that counts(v)
! is the amount of appearances of v. However, that method would occupy
! much more memory than mine if a large number appears.
!
! What I did is to save all the different given values, sorted and non-repeated
! in the array x_axis, so counts(i) is the amount of times x_axis(i) was written.


program histogram
implicit none

integer, allocatable :: a(:), a_sorted(:), x_axis(:), counts(:), l(:)
integer :: ierr, i, n, x, counter

write(*,*) 'Write your numbers, one by one.'
write(*,*) 'Once you have finished, input a letter'
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
if (n == 0) stop 'Empty array given'
allocate(a_sorted(n))

! Let's sort 'a' first
do i = 1, n
  a_sorted(i) = minval(a)
  l = minloc(a)
  a(l) = maxval(a+1)
end do

print*, 'You gave the values:'
print*, a_sorted
print*, 'Creating histogram.......'

allocate(x_axis(1))
allocate(counts(0))
x_axis = a_sorted(1)
counter = 1
do i = 2, n
  if (a_sorted(i) /= x_axis(size(x_axis))) then
    x_axis = [x_axis, a_sorted(i)]
    counts = [counts, counter]
    counter = 0
  end if
  counter = counter + 1
end do
counts = [counts, counter]


print*, '       Value        |     Times it appears:'
print*, ''
do i = 1, size(x_axis)
  print*, x_axis(i), '        |', counts(i)
end do

print*, ''
print*, '---------------------------------------------------------'
print*, ''
print*, 'array from', lbound(counts), 'to', ubound(counts), 'of size', size(counts)
print*, ''
print*, 'The way I did this exercise, ubound() is exactly equal to size()'

end program
