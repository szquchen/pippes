MODULE inv_mgv811
!..use and access
use inv_wp
use inv_mg811
use inv_mg7111
implicit none
private
public :: mgv811_base0, mgv811_base
!..data
integer, parameter, private :: &
  nkk=mg811_nkk, nk=mg811_nk, nr=mg811_nr, &
  nkj(0:nkk-1)=mg811_nkj
integer, parameter, public :: &
  mgv811_nb0(0:9) = mg7111_nb, &
  mgv811_nb(0:9) = mgv811_nb0+2*mg811_nb, &
  mgv811_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,0,8,9, &
      0,2,3,4,5,6,7,1,8,9, &
      0,1,3,4,5,6,7,2,8,9, &
      0,1,2,4,5,6,7,3,8,9, &
      0,1,2,3,5,6,7,4,8,9, &
      0,1,2,3,4,6,7,5,8,9, &
      0,1,2,3,4,5,7,6,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv811_base0.f90'
include 'mgv811_base.f90'
END MODULE inv_mgv811
