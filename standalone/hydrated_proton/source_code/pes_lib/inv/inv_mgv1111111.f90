MODULE inv_mgv1111111
!..use and access
use inv_wp
use inv_mg1111111
implicit none
private
public :: mgv1111111_base
!..data
integer, parameter, private :: &
  nkk=mg1111111_nkk, nk=mg1111111_nk, nr=mg1111111_nr, &
  nkj(0:nkk-1)=mg1111111_nkj
integer, parameter, public :: &
  mgv1111111_nb(0:9) = 7*mg1111111_nb, &
  mgv1111111_iord(0:nk-1,0:nk-1) = reshape( &
    (/0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv1111111_base.f90'
END MODULE inv_mgv1111111
