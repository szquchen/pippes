MODULE inv_mgv32
!..use and access
use inv_wp
use inv_mg32
use inv_mg221
use inv_mg311
implicit none
private
public :: mgv32_base0, mgv32_base1, mgv32_base
!..data
integer, parameter, private :: &
  nkk=mg32_nkk, nk=mg32_nk, nr=mg32_nr, &
  nkj(0:nkk-1)=mg32_nkj
integer, parameter, public :: &
  mgv32_nb0(0:9) = mg221_nb, &
  mgv32_nb1(0:9) = mg311_nb, &
  mgv32_nb(0:9) = mgv32_nb0+mgv32_nb1, &
  mgv32_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,0, &
      0,2,3,4,1, &
      0,1,3,4,2, &
      0,1,2,4,3, &
      0,1,2,3,4/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv32_base0.f90'
include 'mgv32_base1.f90'
include 'mgv32_base.f90'
END MODULE inv_mgv32
