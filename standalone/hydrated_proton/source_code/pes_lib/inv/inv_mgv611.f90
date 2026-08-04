MODULE inv_mgv611
!..use and access
use inv_wp
use inv_mg611
use inv_mg5111
implicit none
private
public :: mgv611_base0, mgv611_base
!..data
integer, parameter, private :: &
  nkk=mg611_nkk, nk=mg611_nk, nr=mg611_nr, &
  nkj(0:nkk-1)=mg611_nkj
integer, parameter, public :: &
  mgv611_nb0(0:9) = mg5111_nb, &
  mgv611_nb(0:9) = mgv611_nb0+2*mg611_nb, &
  mgv611_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,0,6,7, &
      0,2,3,4,5,1,6,7, &
      0,1,3,4,5,2,6,7, &
      0,1,2,4,5,3,6,7, &
      0,1,2,3,5,4,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv611_base0.f90'
include 'mgv611_base.f90'
END MODULE inv_mgv611
