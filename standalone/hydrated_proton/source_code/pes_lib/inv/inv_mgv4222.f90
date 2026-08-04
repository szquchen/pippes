MODULE inv_mgv4222
!..use and access
use inv_wp
use inv_mg4222
use inv_mg32221
use inv_mg42211
implicit none
private
public :: mgv4222_base0, mgv4222_base1, mgv4222_base2, mgv4222_base3, &
  mgv4222_base
!..data
integer, parameter, private :: &
  nkk=mg4222_nkk, nk=mg4222_nk, nr=mg4222_nr, &
  nkj(0:nkk-1)=mg4222_nkj
integer, parameter, public :: &
  mgv4222_nb0(0:9) = mg32221_nb, &
  mgv4222_nb1(0:9) = mg42211_nb, &
  mgv4222_nb2(0:9) = mg42211_nb, &
  mgv4222_nb3(0:9) = mg42211_nb, &
  mgv4222_nb(0:9) = mgv4222_nb0+mgv4222_nb1+mgv4222_nb2+mgv4222_nb3, &
  mgv4222_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,8,9,0, &
      0,2,3,4,5,6,7,8,9,1, &
      0,1,3,4,5,6,7,8,9,2, &
      0,1,2,4,5,6,7,8,9,3, &
      0,1,2,3,6,7,8,9,5,4, &
      0,1,2,3,6,7,8,9,4,5, &
      0,1,2,3,4,5,8,9,7,6, &
      0,1,2,3,4,5,8,9,6,7, &
      0,1,2,3,4,5,6,7,9,8, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv4222_base0.f90'
include 'mgv4222_base1.f90'
include 'mgv4222_base2.f90'
include 'mgv4222_base3.f90'
include 'mgv4222_base.f90'
END MODULE inv_mgv4222
