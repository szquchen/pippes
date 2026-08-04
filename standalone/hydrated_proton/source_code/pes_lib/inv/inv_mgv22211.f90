MODULE inv_mgv22211
!..use and access
use inv_wp
use inv_mg22211
use inv_mg221111
implicit none
private
public :: mgv22211_base0, mgv22211_base1, &
  mgv22211_base2, mgv22211_base
!..data
integer, parameter, private :: &
  nkk=mg22211_nkk, nk=mg22211_nk, nr=mg22211_nr, &
  nkj(0:nkk-1)=mg22211_nkj
integer, parameter, public :: &
  mgv22211_nb0(0:9) = mg221111_nb, &
  mgv22211_nb1(0:9) = mg221111_nb, &
  mgv22211_nb2(0:9) = mg221111_nb, &
  mgv22211_nb(0:9) = mgv22211_nb0+mgv22211_nb1+mgv22211_nb2+2*mg22211_nb, &
  mgv22211_iord(0:nk-1,0:nk-1) = reshape( &
    (/2,3,4,5,1,0,6,7, &
      2,3,4,5,0,1,6,7, &
      0,1,4,5,3,2,6,7, &
      0,1,4,5,2,3,6,7, &
      0,1,2,3,5,4,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv22211_base0.f90'
include 'mgv22211_base1.f90'
include 'mgv22211_base2.f90'
include 'mgv22211_base.f90'
END MODULE inv_mgv22211
