MODULE inv_mgv422
!..use and access
use inv_wp
use inv_mg422
use inv_mg3221
use inv_mg4211
implicit none
private
public :: mgv422_base0, mgv422_base1, mgv422_base
!..data
integer, parameter, private :: &
  nkk=mg422_nkk, nk=mg422_nk, nr=mg422_nr, &
  nkj(0:nkk-1)=mg422_nkj
integer, parameter, public :: &
  mgv422_nb0(0:9) = mg3221_nb, &
  mgv422_nb1(0:9) = mg4211_nb, &
  mgv422_nb2(0:9) = mg4211_nb, &
  mgv422_nb(0:9) = mgv422_nb0+mgv422_nb1+mgv422_nb2, &
  mgv422_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,0, &
      0,2,3,4,5,6,7,1, &
      0,1,3,4,5,6,7,2, &
      0,1,2,4,5,6,7,3, &
      0,1,2,3,6,7,5,4, &
      0,1,2,3,6,7,4,5, &
      0,1,2,3,4,5,7,6, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv422_base0.f90'
include 'mgv422_base1.f90'
include 'mgv422_base2.f90'
include 'mgv422_base.f90'
END MODULE inv_mgv422
