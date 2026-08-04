MODULE inv_mgv11111
!..use and access
use inv_wp
use inv_mg11111
implicit none
private
public :: mgv11111_base
!..data
integer, parameter, private :: &
  nkk=mg11111_nkk, nk=mg11111_nk, nr=mg11111_nr, &
  nkj(0:nkk-1)=mg11111_nkj
integer, parameter, public :: &
  mgv11111_nb(0:9) = 5*mg11111_nb, &
  mgv11111_iord(0:nk-1,0:nk-1) = reshape( &
    (/0,1,2,3,4, &
      0,1,2,3,4, &
      0,1,2,3,4, &
      0,1,2,3,4, &
      0,1,2,3,4/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv11111_base.f90'
END MODULE inv_mgv11111
