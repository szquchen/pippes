MODULE inv_mgv43
!..use and access
use inv_wp
use inv_mg43
use inv_mg331
use inv_mg421
implicit none
private
public :: mgv43_base0, mgv43_base1, mgv43_base
!..data
integer, parameter, private :: &
  nkk=mg43_nkk, nk=mg43_nk, nr=mg43_nr, &
  nkj(0:nkk-1)=mg43_nkj
integer, parameter, public :: &
  mgv43_nb0(0:9) = mg331_nb, &
  mgv43_nb1(0:9) = mg421_nb, &
  mgv43_nb(0:9) = mgv43_nb0+mgv43_nb1, &
  mgv43_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,0, &
      0,2,3,4,5,6,1, &
      0,1,3,4,5,6,2, &
      0,1,2,4,5,6,3, &
      0,1,2,3,5,6,4, &
      0,1,2,3,4,6,5, &
      0,1,2,3,4,5,6/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv43_base0.f90'
include 'mgv43_base1.f90'
include 'mgv43_base.f90'
END MODULE inv_mgv43
