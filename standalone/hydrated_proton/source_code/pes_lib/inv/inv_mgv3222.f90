MODULE inv_mgv3222
!..use and access
use inv_wp
use inv_mg3222
use inv_mg32211
use inv_mg22221
implicit none
private
public :: mgv3222_base0, mgv3222_base1, mgv3222_base2, mgv3222_base3, &
  mgv3222_base
!..data
integer, parameter, private :: &
  nkk=mg3222_nkk, nk=mg3222_nk, nr=mg3222_nr, &
  nkj(0:nkk-1)=mg3222_nkj
integer, parameter, public :: &
  mgv3222_nb0(0:9) = mg22221_nb, &
  mgv3222_nb1(0:9) = mg32211_nb, &
  mgv3222_nb2(0:9) = mg32211_nb, &
  mgv3222_nb3(0:9) = mg32211_nb, &
  mgv3222_nb(0:9) = mgv3222_nb0+mgv3222_nb1+mgv3222_nb2+mgv3222_nb3, &
  mgv3222_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,8,0, &
      0,2,3,4,5,6,7,8,1, &
      0,1,3,4,5,6,7,8,2, &
      0,1,2,5,6,7,8,4,3, &
      0,1,2,5,6,7,8,3,4, &
      0,1,2,3,4,7,8,6,5, &
      0,1,2,3,4,7,8,5,6, &
      0,1,2,3,4,5,6,8,7, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv3222_base0.f90'
include 'mgv3222_base1.f90'
include 'mgv3222_base2.f90'
include 'mgv3222_base3.f90'
include 'mgv3222_base.f90'
END MODULE inv_mgv3222
