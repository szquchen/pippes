MODULE inv_mgv531
!..use and access
use inv_wp
use inv_mg531
use inv_mg5211
use inv_mg4311
implicit none
private
public :: mgv531_base0, mgv531_base1, mgv531_base
!..data
integer, parameter, private :: &
  nkk=mg531_nkk, nk=mg531_nk, nr=mg531_nr, &
  nkj(0:nkk-1)=mg531_nkj
integer, parameter, public :: &
  mgv531_nb0(0:9) = mg4311_nb, &
  mgv531_nb1(0:9) = mg5211_nb, &
  mgv531_nb(0:9) = mgv531_nb0+mgv531_nb1+mg531_nb, &
  mgv531_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,0,8, &
      0,2,3,4,5,6,7,1,8, &
      0,1,3,4,5,6,7,2,8, &
      0,1,2,4,5,6,7,3,8, &
      0,1,2,3,5,6,7,4,8, &
      0,1,2,3,4,6,7,5,8, &
      0,1,2,3,4,5,7,6,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv531_base0.f90'
include 'mgv531_base1.f90'
include 'mgv531_base.f90'
END MODULE inv_mgv531
