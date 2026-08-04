MODULE inv_mgv3331
!..use and access
use inv_wp
use inv_mg3331
use inv_mg33211
implicit none
private
public :: mgv3331_base0, mgv3331_base1, mgv3331_base2, mgv3331_base
!..data
integer, parameter, private :: &
  nkk=mg3331_nkk, nk=mg3331_nk, nr=mg3331_nr, &
  nkj(0:nkk-1)=mg3331_nkj
integer, parameter, public :: &
  mgv3331_nb0(0:9) = mg33211_nb, &
  mgv3331_nb1(0:9) = mg33211_nb, &
  mgv3331_nb2(0:9) = mg33211_nb, &
  mgv3331_nb(0:9) = mgv3331_nb0+mgv3331_nb1+mgv3331_nb2+mg3331_nb, &
  mgv3331_iord(0:nk-1,0:nk-1) = reshape( &
    (/3,4,5,6,7,8,1,2,0,9, &
      3,4,5,6,7,8,0,2,1,9, &
      3,4,5,6,7,8,0,1,2,9, &
      0,1,2,6,7,8,4,5,3,9, &
      0,1,2,6,7,8,3,5,4,9, &
      0,1,2,6,7,8,3,4,5,9, &
      0,1,2,3,4,5,7,8,6,9, &
      0,1,2,3,4,5,6,8,7,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv3331_base0.f90'
include 'mgv3331_base1.f90'
include 'mgv3331_base2.f90'
include 'mgv3331_base.f90'
END MODULE inv_mgv3331
