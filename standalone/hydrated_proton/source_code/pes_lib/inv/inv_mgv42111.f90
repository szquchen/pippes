MODULE inv_mgv42111
!..use and access
use inv_wp
use inv_mg42111
use inv_mg321111
use inv_mg411111
implicit none
private
public :: mgv42111_base0, mgv42111_base1, mgv42111_base
!..data
integer, parameter, private :: &
  nkk=mg42111_nkk, nk=mg42111_nk, nr=mg42111_nr, &
  nkj(0:nkk-1)=mg42111_nkj
integer, parameter, public :: &
  mgv42111_nb0(0:9) = mg321111_nb, &
  mgv42111_nb1(0:9) = mg411111_nb, &
  mgv42111_nb(0:9) = mgv42111_nb0+mgv42111_nb1+3*mg42111_nb, &
  mgv42111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,0,6,7,8, &
      0,2,3,4,5,1,6,7,8, &
      0,1,3,4,5,2,6,7,8, &
      0,1,2,4,5,3,6,7,8, &
      0,1,2,3,5,4,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv42111_base0.f90'
include 'mgv42111_base1.f90'
include 'mgv42111_base.f90'
END MODULE inv_mgv42111
