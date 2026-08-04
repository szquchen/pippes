MODULE inv_mgv1111
!..use and access
use inv_wp
use inv_mg1111
implicit none
private
public :: mgv1111_base
!..data
integer, parameter, private :: &
  nkk=mg1111_nkk, nk=mg1111_nk, nr=mg1111_nr, &
  nkj(0:nkk-1)=mg1111_nkj
integer, parameter, public :: &
  mgv1111_nb(0:19) = 4*mg1111_nb, &
  mgv1111_iord(0:nk-1,0:nk-1) = reshape( &
    (/0,1,2,3, &
      0,1,2,3, &
      0,1,2,3, &
      0,1,2,3/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv1111_base.f90'
END MODULE inv_mgv1111
