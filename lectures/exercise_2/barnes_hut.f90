! Serial Barnes-Hut N-body simulation: the starting code for course
! exercise 2.
!
! Reads the simulation setup from standard input, in the same format as
! the leapfrog.f90 code of exercise 1:
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
!   gfortran -O2 -o barnes_hut barnes_hut.f90
!   ./barnes_hut < input.dat
!
! The particles move with the leapfrog (kick-drift-kick) integrator, as in
! exercise 1, but the accelerations are approximated with an octree
! (Lecture 11). After every drift:
!
!   1. All the particles are put into an octree. The root node is a cube
!      that contains all the particles, and a node that would contain more
!      than one particle is divided into eight child nodes.
!   2. The total mass and the centre of mass of every node are computed,
!      starting from the leaves and moving up the tree.
!   3. The acceleration of each particle is found by walking down the tree
!      from the root. A node of size s whose centre of mass is at distance
!      d from the particle is treated as a single point mass if
!      s / d < theta. Otherwise, its children are visited.
!   4. The tree is deleted.
!
! A smaller opening angle theta gives more accurate accelerations but takes
! longer. With theta = 0, every node is opened and the result is the same
! as the direct sum of exercise 1.
!
! The accelerations are softened, a = m r / (|r|^2 + eps^2)^(3/2), so that
! close encounters between two particles do not give huge accelerations.
program barnes_hut
  use iso_fortran_env, only: real64
  implicit none
  integer, parameter :: dp = real64
  real(dp), parameter :: theta = 0.5_dp     ! opening angle
  real(dp), parameter :: eps = 0.01_dp      ! softening length
  integer, parameter :: max_depth = 50      ! limit for the tree depth

  ! A node of the octree: a cube in space.
  type :: node
    real(dp) :: centre(3)                   ! centre of the cube
    real(dp) :: size                        ! side length of the cube
    integer :: n_particles = 0              ! number of particles in the cube
    integer :: particle = 0                 ! the particle in a leaf (index)
    real(dp) :: mass = 0.0_dp               ! total mass of the particles
    real(dp) :: com(3) = 0.0_dp             ! centre of mass of the particles
    type(node), pointer :: parent => null()
    type(node), pointer :: children(:,:,:) => null()   ! 2x2x2, or null in a leaf
  end type node

  integer :: i, n, step, n_steps, out_every
  real(dp) :: dt, dt_out, t_end
  ! Positions, velocities, and accelerations are stored as (3, n) arrays.
  ! Fortran stores arrays column by column, so the coordinates of one
  ! particle, r(:,i), are next to each other in memory.
  real(dp), allocatable :: m(:), r(:,:), v(:,:), a(:,:)

  read *, dt
  read *, dt_out
  read *, t_end
  read *, n

  allocate(m(n), r(3,n), v(3,n), a(3,n))
  do i = 1, n
    read *, m(i), r(:,i), v(:,i)
  end do

  n_steps = nint(t_end / dt)
  out_every = max(1, nint(dt_out / dt))

  call compute_accelerations(m, r, a)

  do step = 1, n_steps
    v = v + a * dt / 2
    r = r + v * dt
    call compute_accelerations(m, r, a)
    v = v + a * dt / 2

    if (mod(step, out_every) == 0) then
      do i = 1, n
        print *, r(:,i)
      end do
    end if
  end do

  deallocate(m, r, v, a)

contains

  ! Computes the accelerations a of all the particles with masses m at
  ! positions r. Builds the tree, walks it for each particle, and deletes it.
  subroutine compute_accelerations(m, r, a)
    real(dp), intent(in) :: m(:), r(:,:)
    real(dp), intent(out) :: a(:,:)
    type(node), pointer :: root
    integer :: i

    call build_tree(root, r)
    call compute_masses(root, m, r)
    do i = 1, size(m)
      a(:,i) = 0.0_dp
      call add_acceleration(root, i, r, a(:,i))
    end do
    call delete_children(root)
    deallocate(root)
  end subroutine compute_accelerations


  ! Creates a root node that contains all the particles, and inserts the
  ! particles into the tree one at a time.
  subroutine build_tree(root, r)
    type(node), pointer, intent(out) :: root
    real(dp), intent(in) :: r(:,:)
    real(dp) :: lower(3), upper(3)
    integer :: i

    lower = minval(r, dim=2)
    upper = maxval(r, dim=2)

    allocate(root)
    root%centre = (lower + upper) / 2
    ! Make the cube a little larger than needed, so that no particle lies
    ! exactly on its surface. The max() handles the case of one particle.
    root%size = max(1.001_dp * maxval(upper - lower), tiny(1.0_dp))

    do i = 1, size(r, 2)
      call insert_particle(root, i, r, 0)
    end do
  end subroutine build_tree


  ! Inserts particle i into the subtree that starts from node nd.
  ! - An empty node gets the particle.
  ! - A leaf that already has a particle is divided into eight children.
  !   The old particle moves to one of them, and the new particle is
  !   inserted into the child that contains it.
  ! - A node with children passes the particle on to the right child.
  recursive subroutine insert_particle(nd, i, r, depth)
    type(node), target, intent(inout) :: nd
    integer, intent(in) :: i, depth
    real(dp), intent(in) :: r(:,:)
    integer :: k(3)

    if (nd%n_particles == 0) then
      nd%particle = i
    else
      if (.not. associated(nd%children)) then
        ! Two particles at (almost) the same position would be divided
        ! forever, so stop at a maximum depth.
        if (depth >= max_depth) error stop 'Two particles are too close to each other.'
        call subdivide(nd)
        k = child_index(nd, r(:,nd%particle))
        call insert_particle(nd%children(k(1), k(2), k(3)), nd%particle, r, depth + 1)
        nd%particle = 0
      end if
      k = child_index(nd, r(:,i))
      call insert_particle(nd%children(k(1), k(2), k(3)), i, r, depth + 1)
    end if
    nd%n_particles = nd%n_particles + 1
  end subroutine insert_particle


  ! Divides node nd into eight child nodes with half its size.
  subroutine subdivide(nd)
    type(node), target, intent(inout) :: nd
    integer :: ix, iy, iz

    allocate(nd%children(2,2,2))
    do iz = 1, 2
      do iy = 1, 2
        do ix = 1, 2
          nd%children(ix, iy, iz)%size = nd%size / 2
          nd%children(ix, iy, iz)%centre = nd%centre + ([ix, iy, iz] - 1.5_dp) * nd%size / 2
          nd%children(ix, iy, iz)%parent => nd
        end do
      end do
    end do
  end subroutine subdivide


  ! Returns the indices (each 1 or 2) of the child of nd that contains the
  ! point x.
  pure function child_index(nd, x) result(k)
    type(node), intent(in) :: nd
    real(dp), intent(in) :: x(3)
    integer :: k(3)
    k = merge(2, 1, x > nd%centre)
  end function child_index


  ! Computes the total mass and the centre of mass of every node, starting
  ! from the leaves.
  recursive subroutine compute_masses(nd, m, r)
    type(node), intent(inout) :: nd
    real(dp), intent(in) :: m(:), r(:,:)
    integer :: ix, iy, iz

    if (nd%n_particles == 0) return

    if (.not. associated(nd%children)) then
      nd%mass = m(nd%particle)
      nd%com = r(:,nd%particle)
    else
      nd%mass = 0.0_dp
      nd%com = 0.0_dp
      do iz = 1, 2
        do iy = 1, 2
          do ix = 1, 2
            associate (child => nd%children(ix, iy, iz))
              call compute_masses(child, m, r)
              nd%mass = nd%mass + child%mass
              nd%com = nd%com + child%mass * child%com
            end associate
          end do
        end do
      end do
      nd%com = nd%com / nd%mass
    end if
  end subroutine compute_masses


  ! Adds the acceleration that the particles in node nd cause on particle
  ! i to acc.
  recursive subroutine add_acceleration(nd, i, r, acc)
    type(node), intent(in) :: nd
    integer, intent(in) :: i
    real(dp), intent(in) :: r(:,:)
    real(dp), intent(inout) :: acc(3)
    integer :: ix, iy, iz

    if (nd%n_particles == 0) return

    if (.not. associated(nd%children)) then
      ! A leaf: one particle, which may be particle i itself
      if (nd%particle /= i) acc = acc + point_mass_acceleration(nd%mass, nd%com - r(:,i))
    else if (nd%size < theta * norm2(nd%com - r(:,i))) then
      ! A node that is far enough away: treat it as a point mass
      acc = acc + point_mass_acceleration(nd%mass, nd%com - r(:,i))
    else
      ! A node that is too close: visit its children
      do iz = 1, 2
        do iy = 1, 2
          do ix = 1, 2
            call add_acceleration(nd%children(ix, iy, iz), i, r, acc)
          end do
        end do
      end do
    end if
  end subroutine add_acceleration


  ! The softened acceleration caused by a point mass at relative position d.
  pure function point_mass_acceleration(mass, d) result(acc)
    real(dp), intent(in) :: mass, d(3)
    real(dp) :: acc(3)
    acc = mass * d / (sum(d**2) + eps**2)**1.5_dp
  end function point_mass_acceleration


  ! Frees the memory of all the descendants of node nd.
  recursive subroutine delete_children(nd)
    type(node), intent(inout) :: nd
    integer :: ix, iy, iz

    if (.not. associated(nd%children)) return
    do iz = 1, 2
      do iy = 1, 2
        do ix = 1, 2
          call delete_children(nd%children(ix, iy, iz))
        end do
      end do
    end do
    deallocate(nd%children)
  end subroutine delete_children

end program barnes_hut
