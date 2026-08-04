MODULE inv_mgv5311
!..use and access
use inv_wp
use inv_mg5311
use inv_mg52111
use inv_mg43111
implicit none
private
public :: mgv5311_base0, mgv5311_base1, mgv5311_base
!..data
integer, parameter, private :: &
  nkk=mg5311_nkk, nk=mg5311_nk, nr=mg5311_nr, &
  nkj(0:nkk-1)=mg5311_nkj
integer, parameter, public :: &
  mgv5311_nb0(0:9) = mg43111_nb, &
  mgv5311_nb1(0:9) = mg52111_nb, &
  mgv5311_nb(0:9) = mgv5311_nb0+mgv5311_nb1+2*mg5311_nb, &
  mgv5311_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,0,8,9, &
      0,2,3,4,5,6,7,1,8,9, &
      0,1,3,4,5,6,7,2,8,9, &
      0,1,2,4,5,6,7,3,8,9, &
      0,1,2,3,5,6,7,4,8,9, &
      0,1,2,3,4,6,7,5,8,9, &
      0,1,2,3,4,5,7,6,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv5311_base0.f90'
include 'mgv5311_base1.f90'
include 'mgv5311_base.f90'
END MODULE inv_mgv5311
