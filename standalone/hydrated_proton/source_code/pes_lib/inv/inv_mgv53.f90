MODULE inv_mgv53
!..use and access
use inv_wp
use inv_mg53
use inv_mg431
use inv_mg521
implicit none
private
public :: mgv53_base0, mgv53_base1, mgv53_base
!..data
integer, parameter, private :: &
  nkk=mg53_nkk, nk=mg53_nk, nr=mg53_nr, &
  nkj(0:nkk-1)=mg53_nkj
integer, parameter, public :: &
  mgv53_nb0(0:9) = mg431_nb, &
  mgv53_nb1(0:9) = mg521_nb, &
  mgv53_nb(0:9) = mgv53_nb0+mgv53_nb1, &
  mgv53_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,0, &
      0,2,3,4,5,6,7,1, &
      0,1,3,4,5,6,7,2, &
      0,1,2,4,5,6,7,3, &
      0,1,2,3,5,6,7,4, &
      0,1,2,3,4,6,7,5, &
      0,1,2,3,4,5,7,6, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv53_base0.f90'
include 'mgv53_base1.f90'
include 'mgv53_base.f90'
END MODULE inv_mgv53
