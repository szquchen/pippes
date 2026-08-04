MODULE inv_mgv4111
!..use and access
use inv_wp
use inv_mg4111
use inv_mg31111
implicit none
private
public :: mgv4111_base0, mgv4111_base
!..data
integer, parameter, private :: &
  nkk=mg4111_nkk, nk=mg4111_nk, nr=mg4111_nr, &
  nkj(0:nkk-1)=mg4111_nkj
integer, parameter, public :: &
  mgv4111_nb0(0:9) = mg31111_nb, &
  mgv4111_nb(0:9) = mgv4111_nb0+3*mg4111_nb, &
  mgv4111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,0,4,5,6, &
      0,2,3,1,4,5,6, &
      0,1,3,2,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv4111_base0.f90'
include 'mgv4111_base.f90'
END MODULE inv_mgv4111
