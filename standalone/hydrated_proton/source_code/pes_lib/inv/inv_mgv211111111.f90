MODULE inv_mgv211111111
!..use and access
use inv_wp
use inv_mg211111111
use inv_mg1111111111
implicit none
private
public :: mgv211111111_base0, mgv211111111_base
!..data
integer, parameter, private :: &
  nkk=mg211111111_nkk, nk=mg211111111_nk, nr=mg211111111_nr, &
  nkj(0:nkk-1)=mg211111111_nkj
integer, parameter, public :: &
!! max degree reduced to avoid integer overflow
  mgv211111111_nb0(0:7) = mg1111111111_nb(0:7), &
  mgv211111111_nb(0:7) = mgv211111111_nb0(0:7)+8*mg211111111_nb(0:7), &
  mgv211111111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,0,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv211111111_base0.f90'
include 'mgv211111111_base.f90'
END MODULE inv_mgv211111111
