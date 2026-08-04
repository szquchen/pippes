MODULE inv_mgv32211
!..use and access
use inv_wp
use inv_mg32211
use inv_mg321111
use inv_mg222111
implicit none
private
public :: mgv32211_base0, mgv32211_base1, mgv32211_base2, mgv32211_base
!..data
integer, parameter, private :: &
  nkk=mg32211_nkk, nk=mg32211_nk, nr=mg32211_nr, &
  nkj(0:nkk-1)=mg32211_nkj
integer, parameter, public :: &
  mgv32211_nb0(0:9) = mg222111_nb, &
  mgv32211_nb1(0:9) = mg321111_nb, &
  mgv32211_nb2(0:9) = mg321111_nb, &
  mgv32211_nb(0:9) = mgv32211_nb0+mgv32211_nb1+mgv32211_nb2+2*mg32211_nb, &
  mgv32211_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,0,7,8, &
      0,2,3,4,5,6,1,7,8, &
      0,1,3,4,5,6,2,7,8, &
      0,1,2,5,6,4,3,7,8, &
      0,1,2,5,6,3,4,7,8, &
      0,1,2,3,4,6,5,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv32211_base0.f90'
include 'mgv32211_base1.f90'
include 'mgv32211_base2.f90'
include 'mgv32211_base.f90'
END MODULE inv_mgv32211
