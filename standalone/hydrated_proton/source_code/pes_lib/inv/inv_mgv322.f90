MODULE inv_mgv322
!..use and access
use inv_wp
use inv_mg322
use inv_mg2221
use inv_mg3211
implicit none
private
public :: mgv322_base0, mgv322_base1, mgv322_base2, mgv322_base
!..data
integer, parameter, private :: &
  nkk=mg322_nkk, nk=mg322_nk, nr=mg322_nr, &
  nkj(0:nkk-1)=mg322_nkj
integer, parameter, public :: &
  mgv322_nb0(0:9) = mg2221_nb, &
  mgv322_nb1(0:9) = mg3211_nb, &
  mgv322_nb2(0:9) = mg3211_nb, &
  mgv322_nb(0:9) = mgv322_nb0+mgv322_nb1+mgv322_nb2, &
  mgv322_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,0, &
      0,2,3,4,5,6,1, &
      0,1,3,4,5,6,2, &
      0,1,2,5,6,4,3, &
      0,1,2,5,6,3,4, &
      0,1,2,3,4,6,5, &
      0,1,2,3,4,5,6/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv322_base0.f90'
include 'mgv322_base1.f90'
include 'mgv322_base2.f90'
include 'mgv322_base.f90'
END MODULE inv_mgv322
