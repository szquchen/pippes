MODULE inv_mgv331
!..use and access
use inv_wp
use inv_mg331
use inv_mg3211
implicit none
private
public :: mgv331_base0, mgv331_base1, mgv331_base
!..data
integer, parameter, private :: &
  nkk=mg331_nkk, nk=mg331_nk, nr=mg331_nr, &
  nkj(0:nkk-1)=mg331_nkj
integer, parameter, public :: &
  mgv331_nb0(0:9) = mg3211_nb, &
  mgv331_nb1(0:9) = mg3211_nb, &
  mgv331_nb(0:9) = mgv331_nb0+mgv331_nb1+mg331_nb, &
  mgv331_iord(0:nk-1,0:nk-1) = reshape( &
    (/3,4,5,1,2,0,6, &
      3,4,5,0,2,1,6, &
      3,4,5,0,1,2,6, &
      0,1,2,4,5,3,6, &
      0,1,2,3,5,4,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv331_base0.f90'
include 'mgv331_base1.f90'
include 'mgv331_base.f90'
END MODULE inv_mgv331
