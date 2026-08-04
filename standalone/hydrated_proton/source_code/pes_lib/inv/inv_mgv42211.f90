MODULE inv_mgv42211
!..use and access
use inv_wp
use inv_mg42211
use inv_mg322111
use inv_mg421111
implicit none
private
public :: mgv42211_base0, mgv42211_base1, mgv42211_base2, mgv42211_base
!..data
integer, parameter, private :: &
  nkk=mg42211_nkk, nk=mg42211_nk, nr=mg42211_nr, &
  nkj(0:nkk-1)=mg42211_nkj
integer, parameter, public :: &
  mgv42211_nb0(0:9) = mg322111_nb, &
  mgv42211_nb1(0:9) = mg421111_nb, &
  mgv42211_nb2(0:9) = mg421111_nb, &
  mgv42211_nb(0:9) = mgv42211_nb0+mgv42211_nb1+mgv42211_nb2+2*mg42211_nb, &
  mgv42211_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,0,8,9, &
      0,2,3,4,5,6,7,1,8,9, &
      0,1,3,4,5,6,7,2,8,9, &
      0,1,2,4,5,6,7,3,8,9, &
      0,1,2,3,6,7,5,4,8,9, &
      0,1,2,3,6,7,4,5,8,9, &
      0,1,2,3,4,5,7,6,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv42211_base0.f90'
include 'mgv42211_base1.f90'
include 'mgv42211_base2.f90'
include 'mgv42211_base.f90'
END MODULE inv_mgv42211
