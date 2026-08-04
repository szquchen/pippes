MODULE inv_mgv3321
!..use and access
use inv_wp
use inv_mg3321
use inv_mg33111
use inv_mg32211
implicit none
private
public :: mgv3321_base0, mgv3321_base1, mgv3321_base2, mgv3321_base
!..data
integer, parameter, private :: &
  nkk=mg3321_nkk, nk=mg3321_nk, nr=mg3321_nr, &
  nkj(0:nkk-1)=mg3321_nkj
integer, parameter, public :: &
  mgv3321_nb0(0:9) = mg32211_nb, &
  mgv3321_nb1(0:9) = mg32211_nb, &
  mgv3321_nb2(0:9) = mg33111_nb, &
  mgv3321_nb(0:9) = mgv3321_nb0+mgv3321_nb1+mgv3321_nb2+mg3321_nb, &
  mgv3321_iord(0:nk-1,0:nk-1) = reshape( &
    (/3,4,5,1,2,6,7,0,8, &
      3,4,5,0,2,6,7,1,8, &
      3,4,5,0,1,6,7,2,8, &
      0,1,2,4,5,6,7,3,8, &
      0,1,2,3,5,6,7,4,8, &
      0,1,2,3,4,6,7,5,8, &
      0,1,2,3,4,5,7,6,8, &
      0,1,2,3,4,5,6,7,8, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv3321_base0.f90'
include 'mgv3321_base1.f90'
include 'mgv3321_base2.f90'
include 'mgv3321_base.f90'
END MODULE inv_mgv3321
