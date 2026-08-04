MODULE inv_mgv22111111
!..use and access
use inv_wp
use inv_mg22111111
use inv_mg211111111
implicit none
private
public :: mgv22111111_base0, mgv22111111_base1, mgv22111111_base
!..data
integer, parameter, private :: &
  nkk=mg22111111_nkk, nk=mg22111111_nk, nr=mg22111111_nr, &
  nkj(0:nkk-1)=mg22111111_nkj
integer, parameter, public :: &
  mgv22111111_nb0(0:7) = mg211111111_nb(0:7), &
  mgv22111111_nb1(0:7) = mg211111111_nb(0:7), &
  mgv22111111_nb(0:7) = mgv22111111_nb0+mgv22111111_nb1+ &
    6*mg22111111_nb(0:7), &
  mgv22111111_iord(0:nk-1,0:nk-1) = reshape( &
    (/2,3,1,0,4,5,6,7,8,9, &
      2,3,0,1,4,5,6,7,8,9, &
      0,1,3,2,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv22111111_base0.f90'
include 'mgv22111111_base1.f90'
include 'mgv22111111_base.f90'
END MODULE inv_mgv22111111
