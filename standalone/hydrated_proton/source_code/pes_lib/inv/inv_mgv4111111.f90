MODULE inv_mgv4111111
!..use and access
use inv_wp
use inv_mg4111111
use inv_mg31111111
implicit none
private
public :: mgv4111111_base0, mgv4111111_base
!..data
integer, parameter, private :: &
  nkk=mg4111111_nkk, nk=mg4111111_nk, nr=mg4111111_nr, &
  nkj(0:nkk-1)=mg4111111_nkj
integer, parameter, public :: &
  mgv4111111_nb0(0:8) = mg31111111_nb(0:8), &
  mgv4111111_nb(0:8) = mgv4111111_nb0+6*mg4111111_nb(0:8), &
  mgv4111111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,0,4,5,6,7,8,9, &
      0,2,3,1,4,5,6,7,8,9, &
      0,1,3,2,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv4111111_base0.f90'
include 'mgv4111111_base.f90'
END MODULE inv_mgv4111111
