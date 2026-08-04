MODULE inv_mgv1111111111
!..use and access
use inv_wp
use inv_mg1111111111
implicit none
private
public :: mgv1111111111_base
!..data
integer, parameter, private :: &
  nkk=mg1111111111_nkk, nk=mg1111111111_nk, nr=mg1111111111_nr, &
  nkj(0:nkk-1)=mg1111111111_nkj
integer, parameter, public :: &
!!  mgv1111111111_nb(0:9) = 10*mg1111111111_nb, & !! integer overflow
  mgv1111111111_nb(0:7) = 10*mg1111111111_nb(0:7), &
  mgv1111111111_iord(0:nk-1,0:nk-1) = reshape( &
    (/0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv1111111111_base.f90'
END MODULE inv_mgv1111111111
