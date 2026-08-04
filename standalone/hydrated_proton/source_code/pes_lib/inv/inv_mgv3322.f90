MODULE inv_mgv3322
!..use and access
use inv_wp
use inv_mg3322
use inv_mg33211
use inv_mg32221
implicit none
private
public :: mgv3322_base0, mgv3322_base1, mgv3322_base2, mgv3322_base3, &
  mgv3322_base
!..data
integer, parameter, private :: &
  nkk=mg3322_nkk, nk=mg3322_nk, nr=mg3322_nr, &
  nkj(0:nkk-1)=mg3322_nkj
integer, parameter, public :: &
  mgv3322_nb0(0:9) = mg32221_nb, &
  mgv3322_nb1(0:9) = mg32221_nb, &
  mgv3322_nb2(0:9) = mg33211_nb, &
  mgv3322_nb3(0:9) = mg33211_nb, &
  mgv3322_nb(0:9) = mgv3322_nb0+mgv3322_nb1+mgv3322_nb2+mgv3322_nb3, &
  mgv3322_iord(0:nk-1,0:nk-1) = reshape( &
    (/3,4,5,1,2,6,7,8,9,0, &
      3,4,5,0,2,6,7,8,9,1, &
      3,4,5,0,1,6,7,8,9,2, &
      0,1,2,4,5,6,7,8,9,3, &
      0,1,2,3,5,6,7,8,9,4, &
      0,1,2,3,4,6,7,8,9,5, &
      0,1,2,3,4,5,8,9,7,6, &
      0,1,2,3,4,5,8,9,6,7, &
      0,1,2,3,4,5,6,7,9,8, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv3322_base0.f90'
include 'mgv3322_base1.f90'
include 'mgv3322_base2.f90'
include 'mgv3322_base3.f90'
include 'mgv3322_base.f90'
END MODULE inv_mgv3322
