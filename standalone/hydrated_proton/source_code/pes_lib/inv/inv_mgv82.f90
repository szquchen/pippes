MODULE inv_mgv82
!..use and access
use inv_wp
use inv_mg82
use inv_mg811
use inv_mg721
implicit none
private
public :: mgv82_base0, mgv82_base1, mgv82_base
!..data
integer, parameter, private :: &
  nkk=mg82_nkk, nk=mg82_nk, nr=mg82_nr, &
  nkj(0:nkk-1)=mg82_nkj
integer, parameter, public :: &
  mgv82_nb0(0:9) = mg721_nb, &
  mgv82_nb1(0:9) = mg811_nb, &
  mgv82_nb(0:9) = mgv82_nb0+mgv82_nb1, &
  mgv82_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,8,9,0, &
      0,2,3,4,5,6,7,8,9,1, &
      0,1,3,4,5,6,7,8,9,2, &
      0,1,2,4,5,6,7,8,9,3, &
      0,1,2,3,5,6,7,8,9,4, &
      0,1,2,3,4,6,7,8,9,5, &
      0,1,2,3,4,5,7,8,9,6, &
      0,1,2,3,4,5,6,8,9,7, &
      0,1,2,3,4,5,6,7,9,8, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv82_base0.f90'
include 'mgv82_base1.f90'
include 'mgv82_base.f90'
END MODULE inv_mgv82
