MODULE inv_mgv4321
!..use and access
use inv_wp
use inv_mg4321
use inv_mg33211
use inv_mg42211
use inv_mg43111
implicit none
private
public :: mgv4321_base0, mgv4321_base1, mgv4321_base2, mgv4321_base
!..data
integer, parameter, private :: &
  nkk=mg4321_nkk, nk=mg4321_nk, nr=mg4321_nr, &
  nkj(0:nkk-1)=mg4321_nkj
integer, parameter, public :: &
  mgv4321_nb0(0:9) = mg33211_nb, &
  mgv4321_nb1(0:9) = mg42211_nb, &
  mgv4321_nb2(0:9) = mg43111_nb, &
  mgv4321_nb3(0:9) = mg4321_nb, &
  mgv4321_nb(0:9) = mgv4321_nb0+mgv4321_nb1+mgv4321_nb2+mgv4321_nb3, &
  mgv4321_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,8,9,0, &
      0,2,3,4,5,6,7,8,9,1, &
      0,1,3,4,5,6,7,8,9,2, &
      0,1,2,4,5,6,7,8,9,3, &
      0,1,2,3,5,6,7,8,9,4, &
      0,1,2,3,4,6,7,8,9,5, &
      0,1,2,3,4,5,7,8,9,6, &
      0,1,2,3,4,5,6,8,9,7, &
      0,1,2,3,4,5,6,7,9,8, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv4321_base0.f90'
include 'mgv4321_base1.f90'
include 'mgv4321_base2.f90'
include 'mgv4321_base.f90'
END MODULE inv_mgv4321
