MODULE inv_mgv431
!..use and access
use inv_wp
use inv_mg431
use inv_mg3311
use inv_mg4211
implicit none
private
public :: mgv431_base0, mgv431_base1, mgv431_base
!..data
integer, parameter, private :: &
  nkk=mg431_nkk, nk=mg431_nk, nr=mg431_nr, &
  nkj(0:nkk-1)=mg431_nkj
integer, parameter, public :: &
  mgv431_nb0(0:9) = mg3311_nb, &
  mgv431_nb1(0:9) = mg4211_nb, &
  mgv431_nb(0:9) = mgv431_nb0+mgv431_nb1+mg431_nb, &
  mgv431_iord(0:nk-1,0:nk-1) = reshape( &
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
include 'mgv431_base0.f90'
include 'mgv431_base1.f90'
include 'mgv431_base.f90'
END MODULE inv_mgv431
