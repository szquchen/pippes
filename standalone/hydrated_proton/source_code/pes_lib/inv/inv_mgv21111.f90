MODULE inv_mgv21111
!..use and access
use inv_wp
use inv_mg21111
use inv_mg111111
implicit none
private
public :: mgv21111_base
!..data
integer, parameter, private :: &
  nkk=mg21111_nkk, nk=mg21111_nk, nr=mg21111_nr, &
  nkj(0:nkk-1)=mg21111_nkj
integer, parameter, public :: &
  mgv21111_nb0(0:9) = mg111111_nb, &
  mgv21111_nb(0:9) = mgv21111_nb0+4*mg21111_nb, &
  mgv21111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,0,2,3,4,5, &
      0,1,2,3,4,5, &
      0,1,2,3,4,5, &
      0,1,2,3,4,5, &
      0,1,2,3,4,5, &
      0,1,2,3,4,5/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv21111_base0.f90'
include 'mgv21111_base.f90'
END MODULE inv_mgv21111
