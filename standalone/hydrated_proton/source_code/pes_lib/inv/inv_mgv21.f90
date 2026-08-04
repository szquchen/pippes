MODULE inv_mgv21
!..use and access
use inv_wp
use inv_mg21
use inv_mg111
implicit none
private
public :: mg21_secs, &
  mg21_base, mgv21_base0, mgv21_base
!..data
integer, parameter, private :: &
  nkk=mg21_nkk, nk=mg21_nk, nr=mg21_nr, &
  nkj(0:nkk-1)=mg21_nkj
integer, parameter, public :: &
  mgv21_nb0(0:19) = mg111_nb, &
  mgv21_nb(0:19) = mgv21_nb0+mg21_nb, &
  mgv21_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,0,2, &
      0,1,2, &
      0,1,2/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv21_base0.f90'
include 'mgv21_base.f90'
END MODULE inv_mgv21
