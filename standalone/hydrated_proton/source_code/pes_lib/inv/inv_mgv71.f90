MODULE inv_mgv71
!..use and access
use inv_wp
use inv_mg71
use inv_mg611
implicit none
private
public :: mgv71_base0, mgv71_base
!..data
integer, parameter, private :: &
  nkk=mg71_nkk, nk=mg71_nk, nr=mg71_nr, &
  nkj(0:nkk-1)=mg71_nkj
integer, parameter, public :: &
  mgv71_nb0(0:9) = mg611_nb, &
  mgv71_nb(0:9) = mgv71_nb0+mg71_nb, &
  mgv71_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,0,7, &
      0,2,3,4,5,6,1,7, &
      0,1,3,4,5,6,2,7, &
      0,1,2,4,5,6,3,7, &
      0,1,2,3,5,6,4,7, &
      0,1,2,3,4,6,5,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv71_base0.f90'
include 'mgv71_base.f90'
END MODULE inv_mgv71
