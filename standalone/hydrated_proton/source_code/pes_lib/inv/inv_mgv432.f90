MODULE inv_mgv432
!..use and access
use inv_wp
use inv_mg432
use inv_mg3321
use inv_mg4221
use inv_mg4311
implicit none
private
public :: mgv432_base0, mgv432_base1, mgv432_base2, mgv432_base
!..data
integer, parameter, private :: &
  nkk=mg432_nkk, nk=mg432_nk, nr=mg432_nr, &
  nkj(0:nkk-1)=mg432_nkj
integer, parameter, public :: &
  mgv432_nb0(0:9) = mg3321_nb, &
  mgv432_nb1(0:9) = mg4221_nb, &
  mgv432_nb2(0:9) = mg4311_nb, &
  mgv432_nb(0:9) = mgv432_nb0+mgv432_nb1+mgv432_nb2, &
  mgv432_iord(0:nk-1,0:nk-1) = reshape( &
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
include 'mgv432_base0.f90'
include 'mgv432_base1.f90'
include 'mgv432_base2.f90'
include 'mgv432_base.f90'
END MODULE inv_mgv432
