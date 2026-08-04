MODULE inv_any
!..use and access
use inv_wp
implicit none
private
public :: any_base, any_mkprib
!..procedures
CONTAINS
include 'any_base.f90'
include 'any_mkprib.f90'
END MODULE inv_any
