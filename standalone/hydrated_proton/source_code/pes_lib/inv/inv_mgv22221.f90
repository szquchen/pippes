MODULE inv_mgv22221
!..use and access
use inv_wp
use inv_mg22221
use inv_mg222111
implicit none
private
public :: mgv22221_base0, mgv22221_base1, mgv22221_base2, &
  mgv22221_base3, mgv22221_base
!..data
integer, parameter, private :: &
  nkk=mg22221_nkk, nk=mg22221_nk, nr=mg22221_nr, &
  nkj(0:nkk-1)=mg22221_nkj
integer, parameter, public :: &
  mgv22221_nb0(0:9) = mg222111_nb, &
  mgv22221_nb1(0:9) = mg222111_nb, &
  mgv22221_nb2(0:9) = mg222111_nb, &
  mgv22221_nb3(0:9) = mg222111_nb, &
  mgv22221_nb(0:9) = mgv22221_nb0+mgv22221_nb1+mgv22221_nb2+ &
    mgv22221_nb3+mg22221_nb, &
  mgv22221_iord(0:nk-1,0:nk-1) = reshape( &
    (/2,3,4,5,6,7,1,0,8, &
      2,3,4,5,6,7,0,1,8, &
      0,1,4,5,6,7,3,2,8, &
      0,1,4,5,6,7,2,3,8, &
      0,1,2,3,6,7,5,4,8, &
      0,1,2,3,6,7,4,5,8, &
      0,1,2,3,4,5,7,6,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv22221_base0.f90'
include 'mgv22221_base1.f90'
include 'mgv22221_base2.f90'
include 'mgv22221_base3.f90'
include 'mgv22221_base.f90'
END MODULE inv_mgv22221
