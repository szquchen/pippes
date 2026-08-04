MODULE inv_mgv321111
!..use and access
use inv_wp
use inv_mg321111
use inv_mg2211111
use inv_mg3111111
implicit none
private
public :: mgv321111_base0, mgv321111_base1, mgv321111_base
!..data
integer, parameter, private :: &
  nkk=mg321111_nkk, nk=mg321111_nk, nr=mg321111_nr, &
  nkj(0:nkk-1)=mg321111_nkj
integer, parameter, public :: &
  mgv321111_nb0(0:9) = mg2211111_nb, &
  mgv321111_nb1(0:9) = mg3111111_nb, &
  mgv321111_nb(0:9) = mgv321111_nb0+mgv321111_nb1+4*mg321111_nb, &
  mgv321111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,0,5,6,7,8, &
      0,2,3,4,1,5,6,7,8, &
      0,1,3,4,2,5,6,7,8, &
      0,1,2,4,3,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv321111_base0.f90'
include 'mgv321111_base1.f90'
include 'mgv321111_base.f90'
END MODULE inv_mgv321111
