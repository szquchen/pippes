MODULE inv_mgv11111111
!..use and access
use inv_wp
use inv_mg11111111
implicit none
private
public :: mgv11111111_base
!..data
integer, parameter, private :: &
  nkk=mg11111111_nkk, nk=mg11111111_nk, nr=mg11111111_nr, &
  nkj(0:nkk-1)=mg11111111_nkj
integer, parameter, public :: &
  mgv11111111_nb(0:9) = 8*mg11111111_nb, &
  mgv11111111_iord(0:nk-1,0:nk-1) = reshape( &
    (/0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv11111111_base.f90'
END MODULE inv_mgv11111111
