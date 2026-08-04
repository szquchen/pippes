MODULE inv_mgv721
!..use and access
use inv_wp
use inv_mg721
use inv_mg7111
use inv_mg6211
implicit none
private
public :: mgv721_base0, mgv721_base1, mgv721_base
!..data
integer, parameter, private :: &
  nkk=mg721_nkk, nk=mg721_nk, nr=mg721_nr, &
  nkj(0:nkk-1)=mg721_nkj
integer, parameter, public :: &
  mgv721_nb0(0:9) = mg6211_nb, &
  mgv721_nb1(0:9) = mg7111_nb, &
  mgv721_nb(0:9) = mgv721_nb0+mgv721_nb1+mg721_nb, &
  mgv721_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,8,0,9, &
      0,2,3,4,5,6,7,8,1,9, &
      0,1,3,4,5,6,7,8,2,9, &
      0,1,2,4,5,6,7,8,3,9, &
      0,1,2,3,5,6,7,8,4,9, &
      0,1,2,3,4,6,7,8,5,9, &
      0,1,2,3,4,5,7,8,6,9, &
      0,1,2,3,4,5,6,8,7,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv721_base0.f90'
include 'mgv721_base1.f90'
include 'mgv721_base.f90'
END MODULE inv_mgv721
