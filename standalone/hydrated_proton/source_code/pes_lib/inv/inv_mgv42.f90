MODULE inv_mgv42
!..use and access
use inv_wp
use inv_mg42
use inv_mg321
use inv_mg411
implicit none
private
public :: mgv42_base0, mgv42_base1, mgv42_base
!..data
integer, parameter, private :: &
  nkk=mg42_nkk, nk=mg42_nk, nr=mg42_nr, &
  nkj(0:nkk-1)=mg42_nkj
integer, parameter, public :: &
  mgv42_nb0(0:9) = mg321_nb, &
  mgv42_nb1(0:9) = mg411_nb, &
  mgv42_nb(0:9) = mgv42_nb0+mgv42_nb1, &
  mgv42_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,0, &
      0,2,3,4,5,1, &
      0,1,3,4,5,2, &
      0,1,2,4,5,3, &
      0,1,2,3,5,4, &
      0,1,2,3,4,5/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv42_base0.f90'
include 'mgv42_base1.f90'
include 'mgv42_base.f90'
END MODULE inv_mgv42
