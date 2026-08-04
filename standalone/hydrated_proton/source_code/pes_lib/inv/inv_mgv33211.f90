MODULE inv_mgv33211
!..use and access
use inv_wp
use inv_mg33211
use inv_mg331111
use inv_mg322111
implicit none
private
public :: mgv33211_base0, mgv33211_base1, mgv33211_base2, mgv33211_base
!..data
integer, parameter, private :: &
  nkk=mg33211_nkk, nk=mg33211_nk, nr=mg33211_nr, &
  nkj(0:nkk-1)=mg33211_nkj
integer, parameter, public :: &
  mgv33211_nb0(0:9) = mg322111_nb, &
  mgv33211_nb1(0:9) = mg322111_nb, &
  mgv33211_nb2(0:9) = mg331111_nb, &
  mgv33211_nb(0:9) = mgv33211_nb0+mgv33211_nb1+mgv33211_nb2+2*mg33211_nb, &
  mgv33211_iord(0:nk-1,0:nk-1) = reshape( &
    (/3,4,5,1,2,6,7,0,8,9, &
      3,4,5,0,2,6,7,1,8,9, &
      3,4,5,0,1,6,7,2,8,9, &
      0,1,2,4,5,6,7,3,8,9, &
      0,1,2,3,5,6,7,4,8,9, &
      0,1,2,3,4,6,7,5,8,9, &
      0,1,2,3,4,5,7,6,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv33211_base0.f90'
include 'mgv33211_base1.f90'
include 'mgv33211_base2.f90'
include 'mgv33211_base.f90'
END MODULE inv_mgv33211
