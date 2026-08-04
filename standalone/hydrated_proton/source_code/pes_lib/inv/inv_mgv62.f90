MODULE inv_mgv62
!..use and access
use inv_wp
use inv_mg62
use inv_mg521
use inv_mg611
implicit none
private
public :: mgv62_base0, mgv62_base1, mgv62_base
!..data
integer, parameter, private :: &
  nkk=mg62_nkk, nk=mg62_nk, nr=mg62_nr, &
  nkj(0:nkk-1)=mg62_nkj
integer, parameter, public :: &
  mgv62_nb0(0:9) = mg521_nb, &
  mgv62_nb1(0:9) = mg611_nb, &
  mgv62_nb(0:9) = mgv62_nb0+mgv62_nb1, &
  mgv62_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,0, &
      0,2,3,4,5,6,7,1, &
      0,1,3,4,5,6,7,2, &
      0,1,2,4,5,6,7,3, &
      0,1,2,3,5,6,7,4, &
      0,1,2,3,4,6,7,5, &
      0,1,2,3,4,5,7,6, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv62_base0.f90'
include 'mgv62_base1.f90'
include 'mgv62_base.f90'
END MODULE inv_mgv62
