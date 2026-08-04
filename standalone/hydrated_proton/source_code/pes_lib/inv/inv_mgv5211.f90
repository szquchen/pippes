MODULE inv_mgv5211
!..use and access
use inv_wp
use inv_mg5211
use inv_mg51111
use inv_mg42111
implicit none
private
public :: mgv5211_base0, mgv5211_base1, mgv5211_base
!..data
integer, parameter, private :: &
  nkk=mg5211_nkk, nk=mg5211_nk, nr=mg5211_nr, &
  nkj(0:nkk-1)=mg5211_nkj
integer, parameter, public :: &
  mgv5211_nb0(0:9) = mg42111_nb, &
  mgv5211_nb1(0:9) = mg51111_nb, &
  mgv5211_nb(0:9) = mgv5211_nb0+mgv5211_nb1+2*mg5211_nb, &
  mgv5211_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,0,7,8, &
      0,2,3,4,5,6,1,7,8, &
      0,1,3,4,5,6,2,7,8, &
      0,1,2,4,5,6,3,7,8, &
      0,1,2,3,5,6,4,7,8, &
      0,1,2,3,4,6,5,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv5211_base0.f90'
include 'mgv5211_base1.f90'
include 'mgv5211_base.f90'
END MODULE inv_mgv5211
