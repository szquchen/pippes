MODULE inv_mgv4221
!..use and access
use inv_wp
use inv_mg4221
use inv_mg32211
use inv_mg42111
implicit none
private
public :: mgv4221_base0, mgv4221_base1, mgv4221_base2, mgv4221_base
!..data
integer, parameter, private :: &
  nkk=mg4221_nkk, nk=mg4221_nk, nr=mg4221_nr, &
  nkj(0:nkk-1)=mg4221_nkj
integer, parameter, public :: &
  mgv4221_nb0(0:9) = mg32211_nb, &
  mgv4221_nb1(0:9) = mg42111_nb, &
  mgv4221_nb2(0:9) = mg42111_nb, &
  mgv4221_nb(0:9) = mgv4221_nb0+mgv4221_nb1+mgv4221_nb2+mg4221_nb, &
  mgv4221_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,0,8, &
      0,2,3,4,5,6,7,1,8, &
      0,1,3,4,5,6,7,2,8, &
      0,1,2,4,5,6,7,3,8, &
      0,1,2,3,6,7,5,4,8, &
      0,1,2,3,6,7,4,5,8, &
      0,1,2,3,4,5,7,6,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv4221_base0.f90'
include 'mgv4221_base1.f90'
include 'mgv4221_base2.f90'
include 'mgv4221_base.f90'
END MODULE inv_mgv4221
