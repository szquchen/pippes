MODULE inv_mgv22222
!..use and access
use inv_wp
use inv_mg22222
use inv_mg222211
implicit none
private
public :: mgv22222_base0, mgv22222_base1, mgv22222_base2, &
  mgv22222_base3, mgv22222_base4, mgv22222_base
!..data
integer, parameter, private :: &
  nkk=mg22222_nkk, nk=mg22222_nk, nr=mg22222_nr, &
  nkj(0:nkk-1)=mg22222_nkj
integer, parameter, public :: &
  mgv22222_nb0(0:9) = mg222211_nb, &
  mgv22222_nb1(0:9) = mg222211_nb, &
  mgv22222_nb2(0:9) = mg222211_nb, &
  mgv22222_nb3(0:9) = mg222211_nb, &
  mgv22222_nb4(0:9) = mg222211_nb, &
  mgv22222_nb(0:9) = mgv22222_nb0+mgv22222_nb1+mgv22222_nb2+ &
    mgv22222_nb3+mgv22222_nb4, &
  mgv22222_iord(0:nk-1,0:nk-1) = reshape( &
    (/2,3,4,5,6,7,8,9,1,0, &
      2,3,4,5,6,7,8,9,0,1, &
      0,1,4,5,6,7,8,9,3,2, &
      0,1,4,5,6,7,8,9,2,3, &
      0,1,2,3,6,7,8,9,5,4, &
      0,1,2,3,6,7,8,9,4,5, &
      0,1,2,3,4,5,8,9,7,6, &
      0,1,2,3,4,5,8,9,6,7, &
      0,1,2,3,4,5,6,7,9,8, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv22222_base0.f90'
include 'mgv22222_base1.f90'
include 'mgv22222_base2.f90'
include 'mgv22222_base3.f90'
include 'mgv22222_base4.f90'
include 'mgv22222_base.f90'
END MODULE inv_mgv22222
