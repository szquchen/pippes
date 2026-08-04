MODULE inv_mgv61
!..use and access
use inv_wp
use inv_mg61
use inv_mg511
implicit none
private
public :: mgv61_base0, mgv61_base
!..data
integer, parameter, private :: &
  nkk=mg61_nkk, nk=mg61_nk, nr=mg61_nr, &
  nkj(0:nkk-1)=mg61_nkj
integer, parameter, public :: &
  mgv61_nb0(0:9) = mg511_nb, &
  mgv61_nb(0:9) = mgv61_nb0+mg61_nb, &
  mgv61_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,0,6, &
      0,2,3,4,5,1,6, &
      0,1,3,4,5,2,6, &
      0,1,2,4,5,3,6, &
      0,1,2,3,5,4,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv61_base0.f90'
include 'mgv61_base.f90'
END MODULE inv_mgv61
