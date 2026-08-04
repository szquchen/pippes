MODULE inv_mgv6111
!..use and access
use inv_wp
use inv_mg6111
use inv_mg51111
implicit none
private
public :: mgv6111_base0, mgv6111_base
!..data
integer, parameter, private :: &
  nkk=mg6111_nkk, nk=mg6111_nk, nr=mg6111_nr, &
  nkj(0:nkk-1)=mg6111_nkj
integer, parameter, public :: &
  mgv6111_nb0(0:9) = mg51111_nb, &
  mgv6111_nb(0:9) = mgv6111_nb0+3*mg6111_nb, &
  mgv6111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,0,6,7,8, &
      0,2,3,4,5,1,6,7,8, &
      0,1,3,4,5,2,6,7,8, &
      0,1,2,4,5,3,6,7,8, &
      0,1,2,3,5,4,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv6111_base0.f90'
include 'mgv6111_base.f90'
END MODULE inv_mgv6111
