MODULE inv_mgv2111
!..use and access
use inv_wp
use inv_mg2111
use inv_mg11111
implicit none
private
public :: mgv2111_base0, mgv2111_base
!..data
integer, parameter, private :: &
  nkk=mg2111_nkk, nk=mg2111_nk, nr=mg2111_nr, &
  nkj(0:nkk-1)=mg2111_nkj
integer, parameter, public :: &
  mgv2111_nb0(0:9) = mg11111_nb, &
  mgv2111_nb(0:9) = mgv2111_nb0+3*mg2111_nb, &
  mgv2111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,0,2,3,4, &
      0,1,2,3,4, &
      0,1,2,3,4, &
      0,1,2,3,4, &
      0,1,2,3,4/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv2111_base0.f90'
include 'mgv2111_base.f90'
END MODULE inv_mgv2111
