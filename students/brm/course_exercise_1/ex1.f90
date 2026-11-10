
!main program for N-body simulation with leapfrog integrator using modules kinds, geomtry and particle
program ex1
  use kinds, only: dp
  use geometry, only: point3d, vector3d, operator(+), operator(-), operator(*), operator(/)
  use particle, only: particle3d
  implicit none

  integer :: i, j, n, step, n_steps, out_every, ios
  real(dp) :: dt, t_end, dt_out, r2, r3
  character(len=256) :: input_file

  type(particle3d), allocatable :: bodies(:)
  type(vector3d), allocatable :: a(:)
  type(vector3d) :: rji

  ! Read the simulation setup from a file given as a command-line argument
  if (command_argument_count() /= 1) then
     print *, "Usage: ./ex1 <input_file>"
     stop 1
  end if
  call get_command_argument(1, input_file)

  open(unit=10, file=trim(input_file), status='old', action='read', iostat=ios)
  if (ios /= 0) then
     print *, "Error opening input file: ", trim(input_file)
     stop 1
  end if

  read(10, *) dt
  read(10, *) dt_out
  read(10, *) t_end
  read(10, *) n

  allocate(bodies(n))
  allocate(a(n))

  do i = 1, n
     read(10, *) bodies(i)%m, bodies(i)%p%x, bodies(i)%p%y, bodies(i)%p%z, &
                 bodies(i)%v%x, bodies(i)%v%y, bodies(i)%v%z
  end do
  close(10)

  open(unit=20, file='output.dat', status='replace', action='write')

  n_steps = nint(t_end / dt)
  out_every = max(1, nint(dt_out / dt))

  ! initial accelerations calculation
  do i = 1, n
     a(i) = vector3d(0.0_dp, 0.0_dp, 0.0_dp)
  end do
  
  do i = 1, n
     do j = i+1, n
        ! this automatically uses the function subpp thanks to the operator -
        rji = bodies(j)%p - bodies(i)%p
        r2 = rji%x**2 + rji%y**2 + rji%z**2
        r3 = r2 * sqrt(r2)
        
        ! we have implemented component-wise vector addition to avoid needing sumvv/subvv that we have not defined
        a(i)%x = a(i)%x + rji%x * (bodies(j)%m / r3)
        a(i)%y = a(i)%y + rji%y * (bodies(j)%m / r3)
        a(i)%z = a(i)%z + rji%z * (bodies(j)%m / r3)
        
        a(j)%x = a(j)%x - rji%x * (bodies(i)%m / r3)
        a(j)%y = a(j)%y - rji%y * (bodies(i)%m / r3)
        a(j)%z = a(j)%z - rji%z * (bodies(i)%m / r3)
     end do
  end do

  ! leapfrog integration loop
  do step = 1, n_steps
     ! 1.kick component-wise
     do i = 1, n
        bodies(i)%v%x = bodies(i)%v%x + a(i)%x * (dt / 2.0_dp)
        bodies(i)%v%y = bodies(i)%v%y + a(i)%y * (dt / 2.0_dp)
        bodies(i)%v%z = bodies(i)%v%z + a(i)%z * (dt / 2.0_dp)
     end do

     ! 2.drift using sumpv and mulvr
     do i = 1, n
        bodies(i)%p = bodies(i)%p + bodies(i)%v * dt
     end do

     ! force, accelerations from new positions
     do i = 1, n
        a(i) = vector3d(0.0_dp, 0.0_dp, 0.0_dp)
     end do
     
     do i = 1, n
        do j = i+1, n
           rji = bodies(j)%p - bodies(i)%p
           r2 = rji%x**2 + rji%y**2 + rji%z**2
           r3 = r2 * sqrt(r2)
           
           a(i)%x = a(i)%x + rji%x * (bodies(j)%m / r3)
           a(i)%y = a(i)%y + rji%y * (bodies(j)%m / r3)
           a(i)%z = a(i)%z + rji%z * (bodies(j)%m / r3)
           
           a(j)%x = a(j)%x - rji%x * (bodies(i)%m / r3)
           a(j)%y = a(j)%y - rji%y * (bodies(i)%m / r3)
           a(j)%z = a(j)%z - rji%z * (bodies(i)%m / r3)
        end do
     end do

     ! kick again component-wise
     do i = 1, n
        bodies(i)%v%x = bodies(i)%v%x + a(i)%x * (dt / 2.0_dp)
        bodies(i)%v%y = bodies(i)%v%y + a(i)%y * (dt / 2.0_dp)
        bodies(i)%v%z = bodies(i)%v%z + a(i)%z * (dt / 2.0_dp)
     end do

     ! write output in the following format: t p1x p1y p1z p2x p2y p2z ... pnx pny pnz
     if (mod(step, out_every) == 0) then
        write(20, '(f12.6)', advance='no') step * dt
        do i = 1, n
           write(20, '(3(1x, es15.7))', advance='no') bodies(i)%p%x, bodies(i)%p%y, bodies(i)%p%z
        end do
        write(20, *) "" ! Move to the next line
     end if
  end do

  close(20)

end program ex1