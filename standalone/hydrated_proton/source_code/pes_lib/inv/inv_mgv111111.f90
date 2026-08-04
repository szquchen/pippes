MODULE inv_mgv111111
!..use and access
use inv_wp
use inv_mg111111
implicit none
private
public :: mgv111111_base
!..data
integer, parameter, private :: &
  nkk=mg111111_nkk, nk=mg111111_nk, nr=mg111111_nr, &
  nkj(0:nkk-1)=mg111111_nkj
integer, parameter, public :: &
  mgv111111_nb(0:9) = 6*mg111111_nb, &
  mgv111111_iord(0:nk-1,0:nk-1) = reshape( &
    (/0,1,2,3,4,5, &
      0,1,2,3,4,5, &
      0,1,2,3,4,5, &
      0,1,2,3,4,5, &
      0,1,2,3,4,5, &
      0,1,2,3,4,5/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv111111_base.f90'
END MODULE inv_mgv111111
