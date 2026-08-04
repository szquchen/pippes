MODULE inv_mgv32111
!..use and access
use inv_wp
use inv_mg32111
use inv_mg221111
use inv_mg311111
implicit none
private
public :: mgv32111_base0, mgv32111_base1, mgv32111_base
!..data
integer, parameter, private :: &
  nkk=mg32111_nkk, nk=mg32111_nk, nr=mg32111_nr, &
  nkj(0:nkk-1)=mg32111_nkj
integer, parameter, public :: &
  mgv32111_nb0(0:9) = mg221111_nb, &
  mgv32111_nb1(0:9) = mg311111_nb, &
  mgv32111_nb(0:9) = mgv32111_nb0+mgv32111_nb1+3*mg32111_nb, &
  mgv32111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,0,5,6,7, &
      0,2,3,4,1,5,6,7, &
      0,1,3,4,2,5,6,7, &
      0,1,2,4,3,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv32111_base0.f90'
include 'mgv32111_base1.f90'
include 'mgv32111_base.f90'
END MODULE inv_mgv32111
