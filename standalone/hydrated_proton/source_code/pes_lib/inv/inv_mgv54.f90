MODULE inv_mgv54
!..use and access
use inv_wp
use inv_mg54
use inv_mg531
use inv_mg441
implicit none
private
public :: mgv54_base0, mgv54_base1, mgv54_base
!..data
integer, parameter, private :: &
  nkk=mg54_nkk, nk=mg54_nk, nr=mg54_nr, &
  nkj(0:nkk-1)=mg54_nkj
integer, parameter, public :: &
  mgv54_nb0(0:9) = mg441_nb, &
  mgv54_nb1(0:9) = mg531_nb, &
  mgv54_nb(0:9) = mgv54_nb0+mgv54_nb1, &
  mgv54_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,8,0, &
      0,2,3,4,5,6,7,8,1, &
      0,1,3,4,5,6,7,8,2, &
      0,1,2,4,5,6,7,8,3, &
      0,1,2,3,5,6,7,8,4, &
      0,1,2,3,4,6,7,8,5, &
      0,1,2,3,4,5,7,8,6, &
      0,1,2,3,4,5,6,8,7, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv54_base0.f90'
include 'mgv54_base1.f90'
include 'mgv54_base.f90'
END MODULE inv_mgv54
