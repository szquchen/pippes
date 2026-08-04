MODULE inv_mgv2111111
!..use and access
use inv_wp
use inv_mg2111111
use inv_mg11111111
implicit none
private
public :: &
  mg2111111_secs, mg2111111_base, mgv2111111_base0, &
  mgv2111111_base
!..data
integer, parameter, private :: &
  nkk=mg2111111_nkk, nk=mg2111111_nk, nr=mg2111111_nr, &
  nkj(0:nkk-1)=mg2111111_nkj
integer, parameter, public :: &
  mgv2111111_nb0(0:9) = mg11111111_nb, &
  mgv2111111_nb(0:9) = mgv2111111_nb0+6*mg2111111_nb, &
  mgv2111111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,0,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv2111111_base0.f90'
include 'mgv2111111_base.f90'
END MODULE inv_mgv2111111
