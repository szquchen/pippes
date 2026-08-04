MODULE inv_mgv81
!..use and access
use inv_wp
use inv_mg81
use inv_mg711
implicit none
private
public :: mgv81_base0, mgv81_base
!..data
integer, parameter, private :: &
  nkk=mg81_nkk, nk=mg81_nk, nr=mg81_nr, &
  nkj(0:nkk-1)=mg81_nkj
integer, parameter, public :: &
  mgv81_nb0(0:9) = mg711_nb, &
  mgv81_nb(0:9) = mgv81_nb0+mg81_nb, &
  mgv81_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,0,8, &
      0,2,3,4,5,6,7,1,8, &
      0,1,3,4,5,6,7,2,8, &
      0,1,2,4,5,6,7,3,8, &
      0,1,2,3,5,6,7,4,8, &
      0,1,2,3,4,6,7,5,8, &
      0,1,2,3,4,5,7,6,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv81_base0.f90'
include 'mgv81_base.f90'
END MODULE inv_mgv81
