MODULE inv_mgv61111
!..use and access
use inv_wp
use inv_mg61111
use inv_mg511111
implicit none
private
public :: mgv61111_base0, mgv61111_base
!..data
integer, parameter, private :: &
  nkk=mg61111_nkk, nk=mg61111_nk, nr=mg61111_nr, &
  nkj(0:nkk-1)=mg61111_nkj
integer, parameter, public :: &
  mgv61111_nb0(0:9) = mg511111_nb, &
  mgv61111_nb(0:9) = mgv61111_nb0+4*mg61111_nb, &
  mgv61111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,0,6,7,8,9, &
      0,2,3,4,5,1,6,7,8,9, &
      0,1,3,4,5,2,6,7,8,9, &
      0,1,2,4,5,3,6,7,8,9, &
      0,1,2,3,5,4,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv61111_base0.f90'
include 'mgv61111_base.f90'
END MODULE inv_mgv61111
