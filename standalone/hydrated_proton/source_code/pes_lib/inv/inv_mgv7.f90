MODULE inv_mgv7
!..use and access
use inv_wp
use inv_mg7
use inv_mg61
implicit none
private
public :: mgv7_base0, mgv7_base
!..data
integer, parameter, private :: &
  nkk=mg7_nkk, nk=mg7_nk, nr=mg7_nr, &
  nkj(0:nkk-1)=mg7_nkj
integer, parameter, public :: &
  mgv7_nb0(0:9) = mg61_nb, &
  mgv7_nb(0:9) = mgv7_nb0, &
  mgv7_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,0, &
      0,2,3,4,5,6,1, &
      0,1,3,4,5,6,2, &
      0,1,2,4,5,6,3, &
      0,1,2,3,5,6,4, &
      0,1,2,3,4,6,5, &
      0,1,2,3,4,5,6/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv7_base0.f90'
include 'mgv7_base.f90'
END MODULE inv_mgv7
