program pointers
implicit none

integer, target :: a(4), b(10)
integer :: i
integer, pointer :: p(:), q(:)

p => null()
q => null()

do i = 0, 9
  b(i+1) = i
end do

do i = 1,4
  a(i) = 87 - 13 * i
end do

print*, a
print*, b

p => a
q => b

p(1) = 0
print*, a

p => b
q => a

print*, associated(p,b)
print*, associated(q,a)


print*, a
print*, b

allocate(p(6))

print*, '--------------------------------------------------------------------------'
print*, p
print*, b

end program
