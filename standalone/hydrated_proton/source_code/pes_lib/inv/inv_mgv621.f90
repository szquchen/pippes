MODULE inv_mgv621
!..use and access
use inv_wp
use inv_mg621
use inv_mg6111
use inv_mg5211
implicit none
private
public :: mgv621_base0, mgv621_base1, mgv621_base
!..data
integer, parameter, private :: &
  nkk=mg621_nkk, nk=mg621_nk, nr=mg621_nr, &
  nkj(0:nkk-1)=mg621_nkj
integer, parameter, public :: &
  mgv621_nb0(0:9) = mg5211_nb, &
  mgv621_nb1(0:9) = mg6111_nb, &
  mgv621_nb(0:9) = mgv621_nb0+mgv621_nb1+mg621_nb, &
  mgv621_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,0,8, &
      0,2,3,4,5,6,7,1,8, &
      0,1,3,4,5,6,7,2,8, &
      0,1,2,4,5,6,7,3,8, &
      0,1,2,3,5,6,7,4,8, &
      0,1,2,3,4,6,7,5,8, &
      0,1,2,3,4,5,7,6,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv621_base0.f90'
include 'mgv621_base1.f90'
include 'mgv621_base.f90'
END MODULE inv_mgv621
