MODULE inv_mgv332
!..use and access
use inv_wp
use inv_mg332
use inv_mg3221
use inv_mg3311
implicit none
private
public :: mgv332_base0, mgv332_base1, mgv332_base2, mgv332_base
!..data
integer, parameter, private :: &
  nkk=mg332_nkk, nk=mg332_nk, nr=mg332_nr, &
  nkj(0:nkk-1)=mg332_nkj
integer, parameter, public :: &
  mgv332_nb0(0:9) = mg3221_nb, &
  mgv332_nb1(0:9) = mg3221_nb, &
  mgv332_nb2(0:9) = mg3311_nb, &
  mgv332_nb(0:9) = mgv332_nb0+mgv332_nb1+mgv332_nb2, &
  mgv332_iord(0:nk-1,0:nk-1) = reshape( &
    (/3,4,5,1,2,6,7,0, &
      3,4,5,0,2,6,7,1, &
      3,4,5,0,1,6,7,2, &
      0,1,2,4,5,6,7,3, &
      0,1,2,3,5,6,7,4, &
      0,1,2,3,4,6,7,5, &
      0,1,2,3,4,5,7,6, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv332_base0.f90'
include 'mgv332_base1.f90'
include 'mgv332_base2.f90'
include 'mgv332_base.f90'
END MODULE inv_mgv332
