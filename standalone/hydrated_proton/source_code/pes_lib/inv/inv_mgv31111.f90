MODULE inv_mgv31111
!..use and access
use inv_wp
use inv_mg31111
use inv_mg211111
implicit none
private
public :: mgv31111_base0, mgv31111_base
!..data
integer, parameter, private :: &
  nkk=mg31111_nkk, nk=mg31111_nk, nr=mg31111_nr, &
  nkj(0:nkk-1)=mg31111_nkj
integer, parameter, public :: &
  mgv31111_nb0(0:9) = mg211111_nb, &
  mgv31111_nb(0:9) = mgv31111_nb0+4*mg31111_nb, &
  mgv31111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,0,3,4,5,6, &
      0,2,1,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv31111_base0.f90'
include 'mgv31111_base.f90'
END MODULE inv_mgv31111
