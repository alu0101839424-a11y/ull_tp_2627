program windowing
implicit none

real, allocatable, target :: image(:,:)
real, pointer :: window(:,:), seconds(:)
integer :: nx, ny

write(*,*) 'Write nx, ny'
read(*,*) nx, ny
allocate(image(nx, ny))
allocate(seconds(nx/2))
seconds => null()

image = 0.0

window => image(nx/4+1:3*nx/4, ny/4+1:3*ny/4)
print*, shape(window), lbound(window)

window = 1.0
print*, sum(image)  ! As expected, changing the pointer also changes what it points to
! Thus, the result of the sum is just x_shape(window) \times y_shape(window)

seconds => image(2:nx:2, 1)

seconds = 5.0
print*, image(:,1)

deallocate(image)
print*, associated(window)

print*, seconds
print*, ''
print*, window

! For some reason, only certain parts of the pointers change (probably pointing to some
! random place in the memory), while the rest remains as we set them to be

end program
