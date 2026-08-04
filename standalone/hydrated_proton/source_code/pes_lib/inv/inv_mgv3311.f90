MODULE inv_mgv3311
!..use and access
use inv_wp
use inv_mg3311
use inv_mg32111
implicit none
private
public :: mgv3311_base0, mgv3311_base1, mgv3311_base
!..data
integer, parameter, private :: &
  nkk=mg3311_nkk, nk=mg3311_nk, nr=mg3311_nr, &
  nkj(0:nkk-1)=mg3311_nkj
integer, parameter, public :: &
  mgv3311_nb0(0:9) = mg32111_nb, &
  mgv3311_nb1(0:9) = mg32111_nb, &
  mgv3311_nb(0:9) = mgv3311_nb0+mgv3311_nb1+2*mg3311_nb, &
  mgv3311_iord(0:nk-1,0:nk-1) = reshape( &
    (/3,4,5,1,2,0,6,7, &
      3,4,5,0,2,1,6,7, &
      3,4,5,0,1,2,6,7, &
      0,1,2,4,5,3,6,7, &
      0,1,2,3,5,4,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv3311_base0.f90'
include 'mgv3311_base1.f90'
include 'mgv3311_base.f90'
END MODULE inv_mgv3311
