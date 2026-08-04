MODULE inv_mgv222211
!..use and access
use inv_wp
use inv_mg222211
use inv_mg2221111
implicit none
private
public :: mgv222211_base0, mgv222211_base1, mgv222211_base2, &
  mgv222211_base3, mgv222211_base
!..data
integer, parameter, private :: &
  nkk=mg222211_nkk, nk=mg222211_nk, nr=mg222211_nr, &
  nkj(0:nkk-1)=mg222211_nkj
integer, parameter, public :: &
  mgv222211_nb0(0:8) = mg2221111_nb(0:8), &
  mgv222211_nb1(0:8) = mg2221111_nb(0:8), &
  mgv222211_nb2(0:8) = mg2221111_nb(0:8), &
  mgv222211_nb3(0:8) = mg2221111_nb(0:8), &
  mgv222211_nb(0:8) = mgv222211_nb0+mgv222211_nb1+mgv222211_nb2+ &
    mgv222211_nb3+2*mg222211_nb(0:8), &
  mgv222211_iord(0:nk-1,0:nk-1) = reshape( &
    (/2,3,4,5,6,7,1,0,8,9, &
      2,3,4,5,6,7,0,1,8,9, &
      0,1,4,5,6,7,3,2,8,9, &
      0,1,4,5,6,7,2,3,8,9, &
      0,1,2,3,6,7,5,4,8,9, &
      0,1,2,3,6,7,4,5,8,9, &
      0,1,2,3,4,5,7,6,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv222211_base0.f90'
include 'mgv222211_base1.f90'
include 'mgv222211_base2.f90'
include 'mgv222211_base3.f90'
include 'mgv222211_base.f90'
END MODULE inv_mgv222211
