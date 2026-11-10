program Mat_mul
implicit none

integer :: i
integer :: A(3,4), B(4,3), prod(3,3)

A(1,:) = [3,2,4,1]
A(2,:) = [2,4,2,2]
A(3,:) = [1,2,3,7]

B(:,1) = [3,2,1,0]
B(:,2) = [2,4,2,2]
B(:,3) = [4,2,3,1]

print*, '              A'
print*,''
do i = 1,3
  print*, A(i,:)
end do
print*,''

print*, '              B'
print*,''
do i = 1,4
  print*, B(i,:)
end do
print*,''

prod = matmul(A,B)
print*, '             AxB'
print*,''

do i = 1,3
  print*, prod(i,:)
end do

end program
