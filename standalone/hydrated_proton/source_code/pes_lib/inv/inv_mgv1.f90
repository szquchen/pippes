MODULE inv_mgv1
!..use and access
use inv_wp
use inv_mg1
implicit none
private
public :: mgv1_base
!..data
integer, parameter, private :: &
  nkk=mg1_nkk, nk=mg1_nk, nr=mg1_nr, &
  nkj(0:nkk-1)=mg1_nkj
integer, parameter, public :: &
  mgv1_nb(0:19) = mg1_nb, &
  mgv1_iord(0:nk-1,0:nk-1) = reshape( &
    (/0/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv1_base.f90'
END MODULE inv_mgv1
