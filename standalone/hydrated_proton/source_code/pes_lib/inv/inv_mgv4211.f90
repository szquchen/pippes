MODULE inv_mgv4211
!..use and access
use inv_wp
use inv_mg4211
use inv_mg32111
use inv_mg41111
implicit none
private
public :: mgv4211_base0, mgv4211_base1, mgv4211_base
!..data
integer, parameter, private :: &
  nkk=mg4211_nkk, nk=mg4211_nk, nr=mg4211_nr, &
  nkj(0:nkk-1)=mg4211_nkj
integer, parameter, public :: &
  mgv4211_nb0(0:9) = mg32111_nb, &
  mgv4211_nb1(0:9) = mg41111_nb, &
  mgv4211_nb(0:9) = mgv4211_nb0+mgv4211_nb1+2*mg4211_nb, &
  mgv4211_iord(0:nk-1,0:nk-1) = reshape( &
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
include 'mgv4211_base0.f90'
include 'mgv4211_base1.f90'
include 'mgv4211_base.f90'
END MODULE inv_mgv4211
