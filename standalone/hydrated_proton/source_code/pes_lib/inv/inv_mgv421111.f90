MODULE inv_mgv421111
!..use and access
use inv_wp
use inv_mg421111
use inv_mg3211111
use inv_mg4111111
implicit none
private
public :: mgv421111_base0, mgv421111_base1, mgv421111_base
!..data
integer, parameter, private :: &
  nkk=mg421111_nkk, nk=mg421111_nk, nr=mg421111_nr, &
  nkj(0:nkk-1)=mg421111_nkj
integer, parameter, public :: &
  mgv421111_nb0(0:9) = mg3211111_nb, &
  mgv421111_nb1(0:9) = mg4111111_nb, &
  mgv421111_nb(0:9) = mgv421111_nb0+mgv421111_nb1+4*mg421111_nb, &
  mgv421111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,0,6,7,8,9, &
      0,2,3,4,5,1,6,7,8,9, &
      0,1,3,4,5,2,6,7,8,9, &
      0,1,2,4,5,3,6,7,8,9, &
      0,1,2,3,5,4,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv421111_base0.f90'
include 'mgv421111_base1.f90'
include 'mgv421111_base.f90'
END MODULE inv_mgv421111
