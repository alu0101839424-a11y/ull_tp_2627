!this particle module will be used to define the physical properties of a particle in 3D space.

module particle
  use kinds, only: dp
  use geometry, only: point3d, vector3d
  implicit none
  private

  ! we will only need particle3d outside the module
  public :: particle3d

  !the particle type will contain the particle's position velocity and mass
  type :: particle3d
    type(point3d) :: p  !position
    type(vector3d) :: v !velocity vector
    real(dp) :: m       !mass
  end type particle3d

end module particle