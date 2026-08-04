MODULE inv_mgv2211
!..use and access
use inv_wp
use inv_mg2211
use inv_mg21111
implicit none
private
public :: mgv2211_base0, mgv2211_base1, mgv2211_base
!..data
integer, parameter, private :: &
  nkk=mg2211_nkk, nk=mg2211_nk, nr=mg2211_nr, &
  nkj(0:nkk-1)=mg2211_nkj
integer, parameter, public :: &
  mgv2211_nb0(0:9) = mg21111_nb, &
  mgv2211_nb1(0:9) = mg21111_nb, &
  mgv2211_nb(0:9) = mgv2211_nb0+mgv2211_nb1+2*mg2211_nb, &
  mgv2211_iord(0:nk-1,0:nk-1) = reshape( &
    (/2,3,1,0,4,5, &
      2,3,0,1,4,5, &
      0,1,3,2,4,5, &
      0,1,2,3,4,5, &
      0,1,2,3,4,5, &
      0,1,2,3,4,5/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv2211_base0.f90'
include 'mgv2211_base1.f90'
include 'mgv2211_base.f90'
END MODULE inv_mgv2211
