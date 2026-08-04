MODULE inv_mgv541
!..use and access
use inv_wp
use inv_mg541
use inv_mg5311
use inv_mg4411
implicit none
private
public :: mgv541_base0, mgv541_base1, mgv541_base
!..data
integer, parameter, private :: &
  nkk=mg541_nkk, nk=mg541_nk, nr=mg541_nr, &
  nkj(0:nkk-1)=mg541_nkj
integer, parameter, public :: &
  mgv541_nb0(0:9) = mg4411_nb, &
  mgv541_nb1(0:9) = mg5311_nb, &
  mgv541_nb(0:9) = mgv541_nb0+mgv541_nb1+mg541_nb, &
  mgv541_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,8,0,9, &
      0,2,3,4,5,6,7,8,1,9, &
      0,1,3,4,5,6,7,8,2,9, &
      0,1,2,4,5,6,7,8,3,9, &
      0,1,2,3,5,6,7,8,4,9, &
      0,1,2,3,4,6,7,8,5,9, &
      0,1,2,3,4,5,7,8,6,9, &
      0,1,2,3,4,5,6,8,7,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv541_base0.f90'
include 'mgv541_base1.f90'
include 'mgv541_base.f90'
END MODULE inv_mgv541
