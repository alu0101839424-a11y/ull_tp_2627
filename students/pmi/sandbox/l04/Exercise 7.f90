program recursivity

implicit none
integer :: i

print *, '5! =', factorial(5)
print *, '10! =', factorial(10)
print*, factorial(0)

!~ print*, factorial(-1)  ! To test the invalid inputs




print *, 'First:', fibonacci(1)
print *, 'Second:', fibonacci(2)
print*, 'Tenth:', fibonacci(10)

!~ print*, fibonacci(0)  ! To test the invalid inputs

print*, 'First 20 numbers of the sequence'
do i = 1, 20
  print *, fibonacci(i)
end do


contains

  recursive function factorial(n) result(fact)
    integer, intent(in) :: n
    integer :: fact
    if (n==0) then
      fact = 1
    elseif (n<0) then
      stop 'Only positive integers are considered as valid inputs'
    else
      fact = n * factorial(n-1)
    end if
  end function
  
  recursive function fibonacci(n) result(n_number)  ! first position will be 1, not 0, so for n=1, f_1 = 0; for n=2, f_2 = 1; for n=3, f_3 = 1...
    integer, intent(in) :: n
    integer :: n_number
    
    if(n == 1) then
      n_number = 0
    elseif(n == 2) then
      n_number = 1
    elseif (n<1) then
      stop 'Number must be greater or equal than 1'
    else
      n_number = fibonacci(n-1) + fibonacci(n-2)
    end if
    
  end function

end program


