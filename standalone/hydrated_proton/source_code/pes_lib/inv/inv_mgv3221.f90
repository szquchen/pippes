MODULE inv_mgv3221
!..use and access
use inv_wp
use inv_mg3221
use inv_mg22211
use inv_mg32111
implicit none
private
public :: mgv3221_base0, mgv3221_base1, mgv3221_base2, mgv3221_base
!..data
integer, parameter, private :: &
  nkk=mg3221_nkk, nk=mg3221_nk, nr=mg3221_nr, &
  nkj(0:nkk-1)=mg3221_nkj
integer, parameter, public :: &
  mgv3221_nb0(0:9) = mg22211_nb, &
  mgv3221_nb1(0:9) = mg32111_nb, &
  mgv3221_nb2(0:9) = mg32111_nb, &
  mgv3221_nb(0:9) = mgv3221_nb0+mgv3221_nb1+mgv3221_nb2+mg3221_nb, &
  mgv3221_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,0,7, &
      0,2,3,4,5,6,1,7, &
      0,1,3,4,5,6,2,7, &
      0,1,2,5,6,4,3,7, &
      0,1,2,5,6,3,4,7, &
      0,1,2,3,4,6,5,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv3221_base0.f90'
include 'mgv3221_base1.f90'
include 'mgv3221_base2.f90'
include 'mgv3221_base.f90'
END MODULE inv_mgv3221
