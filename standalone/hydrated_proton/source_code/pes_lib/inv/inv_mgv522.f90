MODULE inv_mgv522
!..use and access
use inv_wp
use inv_mg522
use inv_mg5211
use inv_mg4221
implicit none
private
public :: mgv522_base0, mgv522_base1, mgv522_base2, mgv522_base
!..data
integer, parameter, private :: &
  nkk=mg522_nkk, nk=mg522_nk, nr=mg522_nr, &
  nkj(0:nkk-1)=mg522_nkj
integer, parameter, public :: &
  mgv522_nb0(0:9) = mg4221_nb, &
  mgv522_nb1(0:9) = mg5211_nb, &
  mgv522_nb2(0:9) = mg5211_nb, &
  mgv522_nb(0:9) = mgv522_nb0+mgv522_nb1+mgv522_nb2, &
  mgv522_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,8,0, &
      0,2,3,4,5,6,7,8,1, &
      0,1,3,4,5,6,7,8,2, &
      0,1,2,4,5,6,7,8,3, &
      0,1,2,3,5,6,7,8,4, &
      0,1,2,3,4,7,8,6,5, &
      0,1,2,3,4,7,8,5,6, &
      0,1,2,3,4,5,6,8,7, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv522_base0.f90'
include 'mgv522_base1.f90'
include 'mgv522_base2.f90'
include 'mgv522_base.f90'
END MODULE inv_mgv522
