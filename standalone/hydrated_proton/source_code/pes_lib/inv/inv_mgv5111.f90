MODULE inv_mgv5111
!..use and access
use inv_wp
use inv_mg5111
use inv_mg41111
implicit none
private
public :: mgv5111_base0, mgv5111_base
!..data
integer, parameter, private :: &
  nkk=mg5111_nkk, nk=mg5111_nk, nr=mg5111_nr, &
  nkj(0:nkk-1)=mg5111_nkj
integer, parameter, public :: &
  mgv5111_nb0(0:9) = mg41111_nb, &
  mgv5111_nb(0:9) = mgv5111_nb0+3*mg5111_nb, &
  mgv5111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,0,5,6,7, &
      0,2,3,4,1,5,6,7, &
      0,1,3,4,2,5,6,7, &
      0,1,2,4,3,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv5111_base0.f90'
include 'mgv5111_base.f90'
END MODULE inv_mgv5111
