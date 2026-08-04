MODULE inv_mgv3111
!..use and access
use inv_wp
use inv_mg3111
use inv_mg21111
implicit none
private
public :: mgv3111_base0, mgv3111_base
!..data
integer, parameter, private :: &
  nkk=mg3111_nkk, nk=mg3111_nk, nr=mg3111_nr, &
  nkj(0:nkk-1)=mg3111_nkj
integer, parameter, public :: &
  mgv3111_nb0(0:9) = mg21111_nb, &
  mgv3111_nb(0:9) = mgv3111_nb0+3*mg3111_nb, &
  mgv3111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,0,3,4,5, &
      0,2,1,3,4,5, &
      0,1,2,3,4,5, &
      0,1,2,3,4,5, &
      0,1,2,3,4,5, &
      0,1,2,3,4,5/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv3111_base0.f90'
include 'mgv3111_base.f90'
END MODULE inv_mgv3111
