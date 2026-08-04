MODULE inv_mgv32221
!..use and access
use inv_wp
use inv_mg32221
use inv_mg322111
use inv_mg222211
implicit none
private
public :: mgv32221_base0, mgv32221_base1, mgv32221_base2, mgv32221_base3, &
  mgv32221_base
!..data
integer, parameter, private :: &
  nkk=mg32221_nkk, nk=mg32221_nk, nr=mg32221_nr, &
  nkj(0:nkk-1)=mg32221_nkj
integer, parameter, public :: &
  mgv32221_nb0(0:9) = mg222211_nb, &
  mgv32221_nb1(0:9) = mg322111_nb, &
  mgv32221_nb2(0:9) = mg322111_nb, &
  mgv32221_nb3(0:9) = mg322111_nb, &
  mgv32221_nb(0:9) = mgv32221_nb0+mgv32221_nb1+mgv32221_nb2+mgv32221_nb3+ &
    mg32221_nb, &
  mgv32221_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,8,0,9, &
      0,2,3,4,5,6,7,8,1,9, &
      0,1,3,4,5,6,7,8,2,9, &
      0,1,2,5,6,7,8,4,3,9, &
      0,1,2,5,6,7,8,3,4,9, &
      0,1,2,3,4,7,8,6,5,9, &
      0,1,2,3,4,7,8,5,6,9, &
      0,1,2,3,4,5,6,8,7,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv32221_base0.f90'
include 'mgv32221_base1.f90'
include 'mgv32221_base2.f90'
include 'mgv32221_base3.f90'
include 'mgv32221_base.f90'
END MODULE inv_mgv32221
