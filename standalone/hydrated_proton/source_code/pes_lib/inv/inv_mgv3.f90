MODULE inv_mgv3
!..use and access
use inv_wp
use inv_mg3
use inv_mg21
implicit none
private
public :: mg3_secs, &
  mg3_base, mgv3_base0, mgv3_base
!..data
integer, parameter, private :: &
  nkk=mg3_nkk, nk=mg3_nk, nr=mg3_nr, &
  nkj(0:nkk-1)=mg3_nkj
integer, parameter, public :: &
  mgv3_nb0(0:19) = mg21_nb, &
  mgv3_nb(0:19) = mgv3_nb0, &
  mgv3_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,0, &
      0,2,1, &
      0,1,2/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv3_base0.f90'
include 'mgv3_base.f90'
END MODULE inv_mgv3
