MODULE inv_mgv222
!..use and access
use inv_wp
use inv_mg222
use inv_mg2211
implicit none
private
public :: mgv222_base0, mgv222_base1, mgv222_base2, mgv222_base
!..data
integer, parameter, private :: &
  nkk=mg222_nkk, nk=mg222_nk, nr=mg222_nr, &
  nkj(0:nkk-1)=mg222_nkj
integer, parameter, public :: &
  mgv222_nb0(0:9) = mg2211_nb, &
  mgv222_nb1(0:9) = mg2211_nb, &
  mgv222_nb2(0:9) = mg2211_nb, &
  mgv222_nb(0:9) = mgv222_nb0+mgv222_nb1+mgv222_nb2, &
  mgv222_iord(0:nk-1,0:nk-1) = reshape( &
    (/2,3,4,5,1,0, &
      2,3,4,5,0,1, &
      0,1,4,5,3,2, &
      0,1,4,5,2,3, &
      0,1,2,3,5,4, &
      0,1,2,3,4,5/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv222_base0.f90'
include 'mgv222_base1.f90'
include 'mgv222_base2.f90'
include 'mgv222_base.f90'
END MODULE inv_mgv222
