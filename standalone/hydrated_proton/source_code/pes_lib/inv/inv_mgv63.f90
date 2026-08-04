MODULE inv_mgv63
!..use and access
use inv_wp
use inv_mg63
use inv_mg621
use inv_mg531
implicit none
private
public :: mgv63_base0, mgv63_base1, mgv63_base
!..data
integer, parameter, private :: &
  nkk=mg63_nkk, nk=mg63_nk, nr=mg63_nr, &
  nkj(0:nkk-1)=mg63_nkj
integer, parameter, public :: &
  mgv63_nb0(0:9) = mg531_nb, &
  mgv63_nb1(0:9) = mg621_nb, &
  mgv63_nb(0:9) = mgv63_nb0+mgv63_nb1, &
  mgv63_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,8,0, &
      0,2,3,4,5,6,7,8,1, &
      0,1,3,4,5,6,7,8,2, &
      0,1,2,4,5,6,7,8,3, &
      0,1,2,3,5,6,7,8,4, &
      0,1,2,3,4,6,7,8,5, &
      0,1,2,3,4,5,7,8,6, &
      0,1,2,3,4,5,6,8,7, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv63_base0.f90'
include 'mgv63_base1.f90'
include 'mgv63_base.f90'
END MODULE inv_mgv63
