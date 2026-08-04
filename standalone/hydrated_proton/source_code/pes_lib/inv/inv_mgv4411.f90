MODULE inv_mgv4411
!..use and access
use inv_wp
use inv_mg4411
use inv_mg43111
implicit none
private
public :: mgv4411_base0, mgv4411_base1, mgv4411_base
!..data
integer, parameter, private :: &
  nkk=mg4411_nkk, nk=mg4411_nk, nr=mg4411_nr, &
  nkj(0:nkk-1)=mg4411_nkj
integer, parameter, public :: &
  mgv4411_nb0(0:9) = mg43111_nb, &
  mgv4411_nb1(0:9) = mg43111_nb, &
  mgv4411_nb(0:9) = mgv4411_nb0+mgv4411_nb1+2*mg4411_nb, &
  mgv4411_iord(0:nk-1,0:nk-1) = reshape( &
    (/4,5,6,7,1,2,3,0,8,9, &
      4,5,6,7,0,2,3,1,8,9, &
      4,5,6,7,0,1,3,2,8,9, &
      4,5,6,7,0,1,2,3,8,9, &
      0,1,2,3,5,6,7,4,8,9, &
      0,1,2,3,4,6,7,5,8,9, &
      0,1,2,3,4,5,7,6,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv4411_base0.f90'
include 'mgv4411_base1.f90'
include 'mgv4411_base.f90'
END MODULE inv_mgv4411
