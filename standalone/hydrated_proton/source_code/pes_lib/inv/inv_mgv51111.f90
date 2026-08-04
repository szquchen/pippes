MODULE inv_mgv51111
!..use and access
use inv_wp
use inv_mg51111
use inv_mg411111
implicit none
private
public :: mgv51111_base0, mgv51111_base
!..data
integer, parameter, private :: &
  nkk=mg51111_nkk, nk=mg51111_nk, nr=mg51111_nr, &
  nkj(0:nkk-1)=mg51111_nkj
integer, parameter, public :: &
  mgv51111_nb0(0:9) = mg411111_nb, &
  mgv51111_nb(0:9) = mgv51111_nb0+4*mg51111_nb, &
  mgv51111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,0,5,6,7,8, &
      0,2,3,4,1,5,6,7,8, &
      0,1,3,4,2,5,6,7,8, &
      0,1,2,4,3,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv51111_base0.f90'
include 'mgv51111_base.f90'
END MODULE inv_mgv51111
