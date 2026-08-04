MODULE inv_mgv31111111
!..use and access
use inv_wp
use inv_mg31111111
use inv_mg211111111
implicit none
private
public :: mgv31111111_base0, mgv31111111_base
!..data
integer, parameter, private :: &
  nkk=mg31111111_nkk, nk=mg31111111_nk, nr=mg31111111_nr, &
  nkj(0:nkk-1)=mg31111111_nkj
integer, parameter, public :: &
  mgv31111111_nb0(0:8) = mg211111111_nb(0:8), &
  mgv31111111_nb(0:8) = mgv31111111_nb0+7*mg31111111_nb(0:8), &
  mgv31111111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,0,3,4,5,6,7,8,9, &
      0,2,1,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv31111111_base0.f90'
include 'mgv31111111_base.f90'
END MODULE inv_mgv31111111
