MODULE inv_mgv2211111
!..use and access
use inv_wp
use inv_mg2211111
use inv_mg21111111
implicit none
private
public :: mgv2211111_base0, mgv2211111_base1, mgv2211111_base
!..data
integer, parameter, private :: &
  nkk=mg2211111_nkk, nk=mg2211111_nk, nr=mg2211111_nr, &
  nkj(0:nkk-1)=mg2211111_nkj
integer, parameter, public :: &
  mgv2211111_nb0(0:9) = mg21111111_nb, &
  mgv2211111_nb1(0:9) = mg21111111_nb, &
  mgv2211111_nb(0:9) = mgv2211111_nb0+mgv2211111_nb1+5*mg2211111_nb, &
  mgv2211111_iord(0:nk-1,0:nk-1) = reshape( &
    (/2,3,1,0,4,5,6,7,8, &
      2,3,0,1,4,5,6,7,8, &
      0,1,3,2,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv2211111_base0.f90'
include 'mgv2211111_base1.f90'
include 'mgv2211111_base.f90'
END MODULE inv_mgv2211111
