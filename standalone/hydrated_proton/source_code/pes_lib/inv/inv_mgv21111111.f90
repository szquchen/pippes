MODULE inv_mgv21111111
!..use and access
use inv_wp
use inv_mg21111111
use inv_mg111111111
implicit none
private
public :: mg21111111_isecs, &
  mg21111111_secs, &
  mg21111111_base, mgv21111111_base0, mgv21111111_base
!..data
integer, parameter, private :: &
  nkk=mg21111111_nkk, nk=mg21111111_nk, nr=mg21111111_nr, &
  nkj(0:nkk-1)=mg21111111_nkj
integer, parameter, public :: &
  mgv21111111_nb0(0:9) = mg111111111_nb, &
!!  mgv21111111_nb(0:9) = mgv21111111_nb0+7*mg21111111_nb, & !! overflow
  mgv21111111_nb(0:8) = mgv21111111_nb0(0:8)+7*mg21111111_nb(0:8), &
  mgv21111111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,0,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv21111111_base0.f90'
include 'mgv21111111_base.f90'
END MODULE inv_mgv21111111
