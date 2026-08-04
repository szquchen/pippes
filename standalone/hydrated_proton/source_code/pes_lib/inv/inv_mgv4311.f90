MODULE inv_mgv4311
!..use and access
use inv_wp
use inv_mg4311
use inv_mg33111
use inv_mg42111
implicit none
private
public :: mgv4311_base0, mgv4311_base1, mgv4311_base
!..data
integer, parameter, private :: &
  nkk=mg4311_nkk, nk=mg4311_nk, nr=mg4311_nr, &
  nkj(0:nkk-1)=mg4311_nkj
integer, parameter, public :: &
  mgv4311_nb0(0:9) = mg33111_nb, &
  mgv4311_nb1(0:9) = mg42111_nb, &
  mgv4311_nb(0:9) = mgv4311_nb0+mgv4311_nb1+2*mg4311_nb, &
  mgv4311_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,0,7,8, &
      0,2,3,4,5,6,1,7,8, &
      0,1,3,4,5,6,2,7,8, &
      0,1,2,4,5,6,3,7,8, &
      0,1,2,3,5,6,4,7,8, &
      0,1,2,3,4,6,5,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv4311_base0.f90'
include 'mgv4311_base1.f90'
include 'mgv4311_base.f90'
END MODULE inv_mgv4311
