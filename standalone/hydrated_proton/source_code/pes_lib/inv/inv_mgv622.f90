MODULE inv_mgv622
!..use and access
use inv_wp
use inv_mg622
use inv_mg6211
use inv_mg5221
implicit none
private
public :: mgv622_base0, mgv622_base1, mgv622_base2, mgv622_base
!..data
integer, parameter, private :: &
  nkk=mg622_nkk, nk=mg622_nk, nr=mg622_nr, &
  nkj(0:nkk-1)=mg622_nkj
integer, parameter, public :: &
  mgv622_nb0(0:9) = mg5221_nb, &
  mgv622_nb1(0:9) = mg6211_nb, &
  mgv622_nb2(0:9) = mg6211_nb, &
  mgv622_nb(0:9) = mgv622_nb0+mgv622_nb1+mgv622_nb2, &
  mgv622_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,8,9,0, &
      0,2,3,4,5,6,7,8,9,1, &
      0,1,3,4,5,6,7,8,9,2, &
      0,1,2,4,5,6,7,8,9,3, &
      0,1,2,3,5,6,7,8,9,4, &
      0,1,2,3,4,6,7,8,9,5, &
      0,1,2,3,4,5,8,9,7,6, &
      0,1,2,3,4,5,8,9,6,7, &
      0,1,2,3,4,5,6,7,9,8, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv622_base0.f90'
include 'mgv622_base1.f90'
include 'mgv622_base2.f90'
include 'mgv622_base.f90'
END MODULE inv_mgv622
