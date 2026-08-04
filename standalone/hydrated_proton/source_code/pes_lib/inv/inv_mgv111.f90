MODULE inv_mgv111
!..use and access
use inv_wp
use inv_mg111
implicit none
private
public :: mgv111_base
!..data
integer, parameter, private :: &
  nkk=mg111_nkk, nk=mg111_nk, nr=mg111_nr, &
  nkj(0:nkk-1)=mg111_nkj
integer, parameter, public :: &
  mgv111_nb(0:19) = 3*mg111_nb, &
  mgv111_iord(0:nk-1,0:nk-1) = reshape( &
    (/0,1,2, &
      0,1,2, &
      0,1,2/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv111_base.f90'
END MODULE inv_mgv111
