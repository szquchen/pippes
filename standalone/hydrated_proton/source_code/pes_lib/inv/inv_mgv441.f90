MODULE inv_mgv441
!..use and access
use inv_wp
use inv_mg441
use inv_mg4311
implicit none
private
public :: mgv441_base0, mgv441_base1, mgv441_base
!..data
integer, parameter, private :: &
  nkk=mg441_nkk, nk=mg441_nk, nr=mg441_nr, &
  nkj(0:nkk-1)=mg441_nkj
integer, parameter, public :: &
  mgv441_nb0(0:9) = mg4311_nb, &
  mgv441_nb1(0:9) = mg4311_nb, &
  mgv441_nb(0:9) = mgv441_nb0+mgv441_nb1+mg441_nb, &
  mgv441_iord(0:nk-1,0:nk-1) = reshape( &
    (/4,5,6,7,1,2,3,0,8, &
      4,5,6,7,0,2,3,1,8, &
      4,5,6,7,0,1,3,2,8, &
      4,5,6,7,0,1,2,3,8, &
      0,1,2,3,5,6,7,4,8, &
      0,1,2,3,4,6,7,5,8, &
      0,1,2,3,4,5,7,6,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv441_base0.f90'
include 'mgv441_base1.f90'
include 'mgv441_base.f90'
END MODULE inv_mgv441
