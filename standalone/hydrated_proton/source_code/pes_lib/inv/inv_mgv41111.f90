MODULE inv_mgv41111
!..use and access
use inv_wp
use inv_mg41111
use inv_mg311111
implicit none
private
public :: mgv41111_base0, mgv41111_base
!..data
integer, parameter, private :: &
  nkk=mg41111_nkk, nk=mg41111_nk, nr=mg41111_nr, &
  nkj(0:nkk-1)=mg41111_nkj
integer, parameter, public :: &
  mgv41111_nb0(0:9) = mg311111_nb, &
  mgv41111_nb(0:9) = mgv41111_nb0+4*mg41111_nb, &
  mgv41111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,0,4,5,6,7, &
      0,2,3,1,4,5,6,7, &
      0,1,3,2,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv41111_base0.f90'
include 'mgv41111_base.f90'
END MODULE inv_mgv41111
