MODULE inv_mgv331111
!..use and access
use inv_wp
use inv_mg331111
use inv_mg3211111
implicit none
private
public :: mgv331111_base0, mgv331111_base1, mgv331111_base
!..data
integer, parameter, private :: &
  nkk=mg331111_nkk, nk=mg331111_nk, nr=mg331111_nr, &
  nkj(0:nkk-1)=mg331111_nkj
integer, parameter, public :: &
  mgv331111_nb0(0:9) = mg3211111_nb, &
  mgv331111_nb1(0:9) = mg3211111_nb, &
  mgv331111_nb(0:9) = mgv331111_nb0+mgv331111_nb1+4*mg331111_nb, &
  mgv331111_iord(0:nk-1,0:nk-1) = reshape( &
    (/3,4,5,1,2,0,6,7,8,9, &
      3,4,5,0,2,1,6,7,8,9, &
      3,4,5,0,1,2,6,7,8,9, &
      0,1,2,4,5,3,6,7,8,9, &
      0,1,2,3,5,4,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv331111_base0.f90'
include 'mgv331111_base1.f90'
include 'mgv331111_base.f90'
END MODULE inv_mgv331111
