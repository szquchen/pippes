MODULE inv_mgv7111
!..use and access
use inv_wp
use inv_mg7111
use inv_mg61111
implicit none
private
public :: mgv7111_base0, mgv7111_base
!..data
integer, parameter, private :: &
  nkk=mg7111_nkk, nk=mg7111_nk, nr=mg7111_nr, &
  nkj(0:nkk-1)=mg7111_nkj
integer, parameter, public :: &
  mgv7111_nb0(0:9) = mg61111_nb, &
  mgv7111_nb(0:9) = mgv7111_nb0+3*mg7111_nb, &
  mgv7111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,0,7,8,9, &
      0,2,3,4,5,6,1,7,8,9, &
      0,1,3,4,5,6,2,7,8,9, &
      0,1,2,4,5,6,3,7,8,9, &
      0,1,2,3,5,6,4,7,8,9, &
      0,1,2,3,4,6,5,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv7111_base0.f90'
include 'mgv7111_base.f90'
END MODULE inv_mgv7111
