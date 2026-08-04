MODULE pes1c_xyz
!..use and access
use inv, wp=>inv_wp
use pes0
use pes1_xyz
implicit none
private
public :: pes1_init_xyz, pes1_vinit_xyz
CONTAINS
include 'pes1_init_xyz.f90'
include 'pes1_vinit_xyz.f90'
END MODULE pes1c_xyz
