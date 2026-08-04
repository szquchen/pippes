MODULE inv_mgv311111
!..use and access
use inv_wp
use inv_mg311111
use inv_mg2111111
implicit none
private
public :: mgv311111_base0, mgv311111_base
!..data
integer, parameter, private :: &
  nkk=mg311111_nkk, nk=mg311111_nk, nr=mg311111_nr, &
  nkj(0:nkk-1)=mg311111_nkj
integer, parameter, public :: &
  mgv311111_nb0(0:9) = mg2111111_nb, &
  mgv311111_nb(0:9) = mgv311111_nb0+5*mg311111_nb, &
  mgv311111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,0,3,4,5,6,7, &
      0,2,1,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv311111_base0.f90'
include 'mgv311111_base.f90'
END MODULE inv_mgv311111
