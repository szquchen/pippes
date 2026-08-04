MODULE inv_mgv211
!..use and access
use inv_wp
use inv_mg211
use inv_mg1111
implicit none
private
public :: mgv211_base0, mgv211_base
!..data
integer, parameter, private :: &
  nkk=mg211_nkk, nk=mg211_nk, nr=mg211_nr, &
  nkj(0:nkk-1)=mg211_nkj
integer, parameter, public :: &
  mgv211_nb0(0:19) = mg1111_nb, &
  mgv211_nb(0:19) = mgv211_nb0+2*mg211_nb, &
  mgv211_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,0,2,3, &
      0,1,2,3, &
      0,1,2,3, &
      0,1,2,3/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv211_base0.f90'
include 'mgv211_base.f90'
END MODULE inv_mgv211
