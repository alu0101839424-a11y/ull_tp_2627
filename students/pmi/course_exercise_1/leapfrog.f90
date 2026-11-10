! Direct N-body simulation with the leapfrog (kick-drift-kick) integrator.
!
! Reads the simulation setup from standard input:
!
!   dt                  time step
!   dt_out              time between outputs
!   t_end               end time of the simulation
!   n                   number of particles
!   m x y z vx vy vz    mass, position, and velocity (one line per particle)
!
! and prints the positions of all the particles every dt_out. The units
! are chosen so that the gravitational constant G = 1.
!
!   gfortran -o leapfrog leapfrog.f90
!   ./leapfrog < input.dat
program leapfrog
  implicit none
  integer :: i, j, n, step, n_steps, out_every
  real :: dt, t_end, dt_out
  real :: r2, r3

  real, dimension(:), allocatable :: m
  real, dimension(:,:), allocatable :: r, v, a
  real, dimension(3) :: rji

  read *, dt
  read *, dt_out
  read *, t_end
  read *, n

  allocate(m(n))
  allocate(r(n,3))
  allocate(v(n,3))
  allocate(a(n,3))

  do i = 1, n
     read *, m(i), r(i,:), v(i,:)
  end do

  n_steps = nint(t_end / dt)
  out_every = max(1, nint(dt_out / dt))

  a = 0.0
  do i = 1, n
     do j = i+1, n
        rji = r(j,:) - r(i,:)
        r2 = sum(rji**2)
        r3 = r2 * sqrt(r2)
        a(i,:) = a(i,:) + m(j) * rji / r3
        a(j,:) = a(j,:) - m(i) * rji / r3
     end do
  end do

  do step = 1, n_steps
     v = v + a * dt/2
     r = r + v * dt
     a = 0.0
     do i = 1, n
        do j = i+1, n
           rji = r(j,:) - r(i,:)
           r2 = sum(rji**2)
           r3 = r2 * sqrt(r2)
           a(i,:) = a(i,:) + m(j) * rji / r3
           a(j,:) = a(j,:) - m(i) * rji / r3
        end do
     end do

     v = v + a * dt/2

     if (mod(step, out_every) == 0) then
        do i = 1, n
           print *, r(i,:)
        end do
     end if
  end do

end program leapfrog
