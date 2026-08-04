MODULE inv_mgv521
!..use and access
use inv_wp
use inv_mg521
use inv_mg4211
use inv_mg5111
implicit none
private
public :: mgv521_base0, mgv521_base1, mgv521_base
!..data
integer, parameter, private :: &
  nkk=mg521_nkk, nk=mg521_nk, nr=mg521_nr, &
  nkj(0:nkk-1)=mg521_nkj
integer, parameter, public :: &
  mgv521_nb0(0:9) = mg4211_nb, &
  mgv521_nb1(0:9) = mg5111_nb, &
  mgv521_nb(0:9) = mgv521_nb0+mgv521_nb1+mg521_nb, &
  mgv521_iord(0:nk-1,0:nk-1) = reshape( &
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
include 'mgv521_base0.f90'
include 'mgv521_base1.f90'
include 'mgv521_base.f90'
END MODULE inv_mgv521
