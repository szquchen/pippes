MODULE inv_mgv22
!..use and access
use inv_wp
use inv_mg22
use inv_mg211
implicit none
private
public :: mgv22_base0, mgv22_base1, mgv22_base
!..data
integer, parameter, private :: &
  nkk=mg22_nkk, nk=mg22_nk, nr=mg22_nr, &
  nkj(0:nkk-1)=mg22_nkj
integer, parameter, public :: &
  mgv22_nb0(0:19) = mg211_nb, &
  mgv22_nb1(0:19) = mg211_nb, &
  mgv22_nb(0:19) = mgv22_nb0+mgv22_nb1, &
  mgv22_iord(0:nk-1,0:nk-1) = reshape( &
    (/2,3,1,0, &
      2,3,0,1, &
      0,1,3,2, &
      0,1,2,3/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv22_base0.f90'
include 'mgv22_base1.f90'
include 'mgv22_base.f90'
END MODULE inv_mgv22
