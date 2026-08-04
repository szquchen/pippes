MODULE inv_mgv442
!..use and access
use inv_wp
use inv_mg442
use inv_mg4321
use inv_mg4411
implicit none
private
public :: mgv442_base0, mgv442_base1, mgv442_base2, mgv442_base
!..data
integer, parameter, private :: &
  nkk=mg442_nkk, nk=mg442_nk, nr=mg442_nr, &
  nkj(0:nkk-1)=mg442_nkj
integer, parameter, public :: &
  mgv442_nb0(0:9) = mg4321_nb, &
  mgv442_nb1(0:9) = mg4321_nb, &
  mgv442_nb2(0:9) = mg4411_nb, &
  mgv442_nb(0:9) = mgv442_nb0+mgv442_nb1+mgv442_nb2, &
  mgv442_iord(0:nk-1,0:nk-1) = reshape( &
    (/4,5,6,7,1,2,3,8,9,0, &
      4,5,6,7,0,2,3,8,9,1, &
      4,5,6,7,0,1,3,8,9,2, &
      4,5,6,7,0,1,2,8,9,3, &
      0,1,2,3,5,6,7,8,9,4, &
      0,1,2,3,4,6,7,8,9,5, &
      0,1,2,3,4,5,7,8,9,6, &
      0,1,2,3,4,5,6,8,9,7, &
      0,1,2,3,4,5,6,7,9,8, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv442_base0.f90'
include 'mgv442_base1.f90'
include 'mgv442_base2.f90'
include 'mgv442_base.f90'
END MODULE inv_mgv442
