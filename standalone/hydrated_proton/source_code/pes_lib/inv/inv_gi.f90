MODULE inv_gi
!..use and access
use inv_wp
implicit none
private
public :: gi_grng, gi_gmod
!..procedures
CONTAINS
include 'gi_grng.f90'
include 'gi_gmod.f90'
END MODULE inv_gi
