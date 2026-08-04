MODULE inv_mgv8
!..use and access
use inv_wp
use inv_mg8
use inv_mg71
implicit none
private
public :: mgv8_base0, mgv8_base
!..data
integer, parameter, public :: &
  mgv8_nb0(0:9) = mg71_nb, &
  mgv8_nb(0:9) = mgv71_nb0, &
  mgv8_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,0, &
      0,2,3,4,5,6,7,1, &
      0,1,3,4,5,6,7,2, &
      0,1,2,4,5,6,7,3, &
      0,1,2,3,5,6,7,4, &
      0,1,2,3,4,6,7,5, &
      0,1,2,3,4,5,7,6, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv8_base0.f90'
include 'mgv8_base.f90'
END MODULE inv_mgv8
