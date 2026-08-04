MODULE inv_mgv222111
!..use and access
use inv_wp
use inv_mg222111
use inv_mg2211111
implicit none
private
public :: mgv222111_base0, mgv222111_base1, mgv222111_base2, &
  mgv222111_base
!..data
integer, parameter, private :: &
  nkk=mg222111_nkk, nk=mg222111_nk, nr=mg222111_nr, &
  nkj(0:nkk-1)=mg222111_nkj
integer, parameter, public :: &
  mgv222111_nb0(0:9) = mg2211111_nb, &
  mgv222111_nb1(0:9) = mg2211111_nb, &
  mgv222111_nb2(0:9) = mg2211111_nb, &
  mgv222111_nb(0:9) = mgv222111_nb0+mgv222111_nb1+mgv222111_nb2+ &
    3*mg222111_nb, &
  mgv222111_iord(0:nk-1,0:nk-1) = reshape( &
    (/2,3,4,5,1,0,6,7,8, &
      2,3,4,5,0,1,6,7,8, &
      0,1,4,5,3,2,6,7,8, &
      0,1,4,5,2,3,6,7,8, &
      0,1,2,3,5,4,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv222111_base0.f90'
include 'mgv222111_base1.f90'
include 'mgv222111_base2.f90'
include 'mgv222111_base.f90'
END MODULE inv_mgv222111
