MODULE inv_mgv321
!..use and access
use inv_wp
use inv_mg321
use inv_mg2211
use inv_mg3111
implicit none
private
public :: mgv321_base0, mgv321_base1, mgv321_base
!..data
integer, parameter, private :: &
  nkk=mg321_nkk, nk=mg321_nk, nr=mg321_nr, &
  nkj(0:nkk-1)=mg321_nkj
integer, parameter, public :: &
  mgv321_nb0(0:9) = mg2211_nb, &
  mgv321_nb1(0:9) = mg3111_nb, &
  mgv321_nb(0:9) = mgv321_nb0+mgv321_nb1+mg321_nb, &
  mgv321_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,0,5, &
      0,2,3,4,1,5, &
      0,1,3,4,2,5, &
      0,1,2,4,3,5, &
      0,1,2,3,4,5, &
      0,1,2,3,4,5/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv321_base0.f90'
include 'mgv321_base1.f90'
include 'mgv321_base.f90'
END MODULE inv_mgv321
