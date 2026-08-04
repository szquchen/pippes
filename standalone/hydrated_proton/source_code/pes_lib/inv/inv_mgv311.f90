MODULE inv_mgv311
!..use and access
use inv_wp
use inv_mg311
use inv_mg2111
implicit none
private
public :: mgv311_base0, mgv311_base
!..data
integer, parameter, private :: &
  nkk=mg311_nkk, nk=mg311_nk, nr=mg311_nr, &
  nkj(0:nkk-1)=mg311_nkj
integer, parameter, public :: &
  mgv311_nb0(0:9) = mg2111_nb, &
  mgv311_nb(0:9) = mgv311_nb0+2*mg311_nb, &
  mgv311_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,0,3,4, &
      0,2,1,3,4, &
      0,1,2,3,4, &
      0,1,2,3,4, &
      0,1,2,3,4/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv311_base0.f90'
include 'mgv311_base.f90'
END MODULE inv_mgv311
