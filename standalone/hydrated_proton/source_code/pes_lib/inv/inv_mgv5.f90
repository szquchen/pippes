MODULE inv_mgv5
!..use and access
use inv_wp
use inv_mg5
use inv_mg41
implicit none
private
public :: mgv5_base0, mgv5_base
!..data
integer, parameter, private :: &
  nkk=mg5_nkk, nk=mg5_nk, nr=mg5_nr, &
  nkj(0:nkk-1)=mg5_nkj
integer, parameter, public :: &
  mgv5_nb0(0:9) = mg41_nb, &
  mgv5_nb(0:9) = mgv5_nb0, &
  mgv5_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,0, &
      0,2,3,4,1, &
      0,1,3,4,2, &
      0,1,2,4,3, &
      0,1,2,3,4/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv5_base0.f90'
include 'mgv5_base.f90'
END MODULE inv_mgv5
