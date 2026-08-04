MODULE inv_mgv3211
!..use and access
use inv_wp
use inv_mg3211
use inv_mg22111
use inv_mg31111
implicit none
private
public :: mgv3211_base0, mgv3211_base1, mgv3211_base
!..data
integer, parameter, private :: &
  nkk=mg3211_nkk, nk=mg3211_nk, nr=mg3211_nr, &
  nkj(0:nkk-1)=mg3211_nkj
integer, parameter, public :: &
  mgv3211_nb0(0:9) = mg22111_nb, &
  mgv3211_nb1(0:9) = mg31111_nb, &
  mgv3211_nb(0:9) = mgv3211_nb0+mgv3211_nb1+2*mg3211_nb, &
  mgv3211_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,0,5,6, &
      0,2,3,4,1,5,6, &
      0,1,3,4,2,5,6, &
      0,1,2,4,3,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv3211_base0.f90'
include 'mgv3211_base1.f90'
include 'mgv3211_base.f90'
END MODULE inv_mgv3211
