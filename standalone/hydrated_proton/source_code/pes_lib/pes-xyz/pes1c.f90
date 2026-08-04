MODULE pes1c
!..use and access
use inv, wp=>inv_wp
use pes1c_xyz
implicit none
private
public :: pes1_init, pes1_vinit
!..procedures
CONTAINS
include 'pes1_init.f90'
include 'pes1_vinit.f90'
END MODULE pes1c
