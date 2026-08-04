MODULE inv_mgv5221
!..use and access
use inv_wp
use inv_mg5221
use inv_mg52111
use inv_mg42211
implicit none
private
public :: mgv5221_base0, mgv5221_base1, mgv5221_base2, mgv5221_base
!..data
integer, parameter, private :: &
  nkk=mg5221_nkk, nk=mg5221_nk, nr=mg5221_nr, &
  nkj(0:nkk-1)=mg5221_nkj
integer, parameter, public :: &
  mgv5221_nb0(0:9) = mg42211_nb, &
  mgv5221_nb1(0:9) = mg52111_nb, &
  mgv5221_nb2(0:9) = mg52111_nb, &
  mgv5221_nb(0:9) = mgv5221_nb0+mgv5221_nb1+mgv5221_nb2+mg5221_nb, &
  mgv5221_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,8,0,9, &
      0,2,3,4,5,6,7,8,1,9, &
      0,1,3,4,5,6,7,8,2,9, &
      0,1,2,4,5,6,7,8,3,9, &
      0,1,2,3,5,6,7,8,4,9, &
      0,1,2,3,4,7,8,6,5,9, &
      0,1,2,3,4,7,8,5,6,9, &
      0,1,2,3,4,5,6,8,7,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv5221_base0.f90'
include 'mgv5221_base1.f90'
include 'mgv5221_base2.f90'
include 'mgv5221_base.f90'
END MODULE inv_mgv5221
