MODULE inv_mgv3211111
!..use and access
use inv_wp
use inv_mg3211111
use inv_mg22111111
use inv_mg31111111
implicit none
private
public :: mgv3211111_base0, mgv3211111_base1, mgv3211111_base
!..data
integer, parameter, private :: &
  nkk=mg3211111_nkk, nk=mg3211111_nk, nr=mg3211111_nr, &
  nkj(0:nkk-1)=mg3211111_nkj
integer, parameter, public :: &
  mgv3211111_nb0(0:8) = mg22111111_nb(0:8), &
  mgv3211111_nb1(0:8) = mg31111111_nb(0:8), &
  mgv3211111_nb(0:8) = mgv3211111_nb0+mgv3211111_nb1+5*mg3211111_nb(0:8), &
  mgv3211111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,0,5,6,7,8,9, &
      0,2,3,4,1,5,6,7,8,9, &
      0,1,3,4,2,5,6,7,8,9, &
      0,1,2,4,3,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv3211111_base0.f90'
include 'mgv3211111_base1.f90'
include 'mgv3211111_base.f90'
END MODULE inv_mgv3211111
