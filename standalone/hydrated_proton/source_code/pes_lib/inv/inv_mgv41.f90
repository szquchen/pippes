MODULE inv_mgv41
!..use and access
use inv_wp
use inv_mg41
use inv_mg311
implicit none
private
public :: mgv41_base0, mgv41_base
!..data
integer, parameter, private :: &
  nkk=mg41_nkk, nk=mg41_nk, nr=mg41_nr, &
  nkj(0:nkk-1)=mg41_nkj
integer, parameter, public :: &
  mgv41_nb0(0:9) = mg311_nb, &
  mgv41_nb(0:9) = mgv41_nb0+mg41_nb, &
  mgv41_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,0,4, &
      0,2,3,1,4, &
      0,1,3,2,4, &
      0,1,2,3,4, &
      0,1,2,3,4/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv41_base0.f90'
include 'mgv41_base.f90'
END MODULE inv_mgv41
