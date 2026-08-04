MODULE inv_mgv6211
!..use and access
use inv_wp
use inv_mg6211
use inv_mg61111
use inv_mg52111
implicit none
private
public :: mgv6211_base0, mgv6211_base1, mgv6211_base
!..data
integer, parameter, private :: &
  nkk=mg6211_nkk, nk=mg6211_nk, nr=mg6211_nr, &
  nkj(0:nkk-1)=mg6211_nkj
integer, parameter, public :: &
  mgv6211_nb0(0:9) = mg52111_nb, &
  mgv6211_nb1(0:9) = mg61111_nb, &
  mgv6211_nb(0:9) = mgv6211_nb0+mgv6211_nb1+2*mg6211_nb, &
  mgv6211_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,0,8,9, &
      0,2,3,4,5,6,7,1,8,9, &
      0,1,3,4,5,6,7,2,8,9, &
      0,1,2,4,5,6,7,3,8,9, &
      0,1,2,3,5,6,7,4,8,9, &
      0,1,2,3,4,6,7,5,8,9, &
      0,1,2,3,4,5,7,6,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv6211_base0.f90'
include 'mgv6211_base1.f90'
include 'mgv6211_base.f90'
END MODULE inv_mgv6211
