MODULE inv_mgv2222
!..use and access
use inv_wp
use inv_mg2222
use inv_mg22211
implicit none
private
public :: mgv2222_base0, mgv2222_base1, &
  mgv2222_base2, mgv2222_base3, mgv2222_base
!..data
integer, parameter, private :: &
  nkk=mg2222_nkk, nk=mg2222_nk, nr=mg2222_nr, &
  nkj(0:nkk-1)=mg2222_nkj
integer, parameter, public :: &
  mgv2222_nb0(0:9) = mg22211_nb, &
  mgv2222_nb1(0:9) = mg22211_nb, &
  mgv2222_nb2(0:9) = mg22211_nb, &
  mgv2222_nb3(0:9) = mg22211_nb, &
  mgv2222_nb(0:9) = mgv2222_nb0+mgv2222_nb1+mgv2222_nb2+mgv2222_nb3, &
  mgv2222_iord(0:nk-1,0:nk-1) = reshape( &
    (/2,3,4,5,6,7,1,0, &
      2,3,4,5,6,7,0,1, &
      0,1,4,5,6,7,3,2, &
      0,1,4,5,6,7,2,3, &
      0,1,2,3,6,7,5,4, &
      0,1,2,3,6,7,4,5, &
      0,1,2,3,4,5,7,6, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv2222_base0.f90'
include 'mgv2222_base1.f90'
include 'mgv2222_base2.f90'
include 'mgv2222_base3.f90'
include 'mgv2222_base.f90'
END MODULE inv_mgv2222
