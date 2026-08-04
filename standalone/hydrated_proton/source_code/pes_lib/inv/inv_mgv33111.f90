MODULE inv_mgv33111
!..use and access
use inv_wp
use inv_mg33111
use inv_mg321111
implicit none
private
public :: mgv33111_base0, mgv33111_base1, mgv33111_base
!..data
integer, parameter, private :: &
  nkk=mg33111_nkk, nk=mg33111_nk, nr=mg33111_nr, &
  nkj(0:nkk-1)=mg33111_nkj
integer, parameter, public :: &
  mgv33111_nb0(0:9) = mg321111_nb, &
  mgv33111_nb1(0:9) = mg321111_nb, &
  mgv33111_nb(0:9) = mgv33111_nb0+mgv33111_nb1+3*mg33111_nb, &
  mgv33111_iord(0:nk-1,0:nk-1) = reshape( &
    (/3,4,5,1,2,0,6,7,8, &
      3,4,5,0,2,1,6,7,8, &
      3,4,5,0,1,2,6,7,8, &
      0,1,2,4,5,3,6,7,8, &
      0,1,2,3,5,4,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv33111_base0.f90'
include 'mgv33111_base1.f90'
include 'mgv33111_base.f90'
END MODULE inv_mgv33111
