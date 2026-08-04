MODULE inv_mgv33
!..use and access
use inv_wp
use inv_mg33
use inv_mg321
implicit none
private
public :: mgv33_base0, mgv33_base1, mgv33_base
!..data
integer, parameter, private :: &
  nkk=mg33_nkk, nk=mg33_nk, nr=mg33_nr, &
  nkj(0:nkk-1)=mg33_nkj
integer, parameter, public :: &
  mgv33_nb0(0:9) = mg321_nb, &
  mgv33_nb1(0:9) = mg321_nb, &
  mgv33_nb(0:9) = mgv33_nb0+mgv33_nb1, &
  mgv33_iord(0:nk-1,0:nk-1) = reshape( &
    (/3,4,5,1,2,0, &
      3,4,5,0,2,1, &
      3,4,5,0,1,2, &
      0,1,2,4,5,3, &
      0,1,2,3,5,4, &
      0,1,2,3,4,5/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv33_base0.f90'
include 'mgv33_base1.f90'
include 'mgv33_base.f90'
END MODULE inv_mgv33
