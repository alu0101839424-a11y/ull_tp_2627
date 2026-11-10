program isit_palindrome
implicit none

integer, parameter :: ik = selected_int_kind(30)
integer(ik) :: num, i, palindro, copy, num_act, n
logical :: itis

write(*,*) 'Write your number'
read(*,*) num

copy = num
i = 0

!   Loop for counting number of digits

do while (abs(copy)>0)
  copy = copy / 10
  i = i + 1
end do

copy = num
n = i ! number of digits

do while (i > 0)
  i = i - 1
  num_act = copy / 10**(i)
  copy = copy - num_act * 10**(i)
  palindro = palindro + num_act * 10**(n - i - 1)
end do

itis = (palindro == num)
if (itis) then
  write(*,*) 'It is a palindrome!!'
  write(*,*) num, '=', palindro
else
  write(*,*) 'It is not a palindrome'
  write(*,*) num, '/=', palindro
end if

end program
