MODULE inv_mgv631
!..use and access
use inv_wp
use inv_mg631
use inv_mg6211
use inv_mg5311
implicit none
private
public :: mgv631_base0, mgv631_base1, mgv631_base
!..data
integer, parameter, private :: &
  nkk=mg631_nkk, nk=mg631_nk, nr=mg631_nr, &
  nkj(0:nkk-1)=mg631_nkj
integer, parameter, public :: &
  mgv631_nb0(0:9) = mg5311_nb, &
  mgv631_nb1(0:9) = mg6211_nb, &
  mgv631_nb(0:9) = mgv631_nb0+mgv631_nb1+mg631_nb, &
  mgv631_iord(0:nk-1,0:nk-1) = reshape( &
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
include 'mgv631_base0.f90'
include 'mgv631_base1.f90'
include 'mgv631_base.f90'
END MODULE inv_mgv631
