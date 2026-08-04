MODULE inv_mgv411111
!..use and access
use inv_wp
use inv_mg411111
use inv_mg3111111
implicit none
private
public :: mgv411111_base0, mgv411111_base
!..data
integer, parameter, private :: &
  nkk=mg411111_nkk, nk=mg411111_nk, nr=mg411111_nr, &
  nkj(0:nkk-1)=mg411111_nkj
integer, parameter, public :: &
  mgv411111_nb0(0:9) = mg3111111_nb, &
  mgv411111_nb(0:9) = mgv411111_nb0+5*mg411111_nb, &
  mgv411111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,0,4,5,6,7,8, &
      0,2,3,1,4,5,6,7,8, &
      0,1,3,2,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv411111_base0.f90'
include 'mgv411111_base.f90'
END MODULE inv_mgv411111
