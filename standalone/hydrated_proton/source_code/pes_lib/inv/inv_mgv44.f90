MODULE inv_mgv44
!..use and access
use inv_wp
use inv_mg44
use inv_mg431
implicit none
private
public :: mgv44_base0, mgv44_base1, mgv44_base
!..data
integer, parameter, private :: &
  nkk=mg44_nkk, nk=mg44_nk, nr=mg44_nr, &
  nkj(0:nkk-1)=mg44_nkj
integer, parameter, public :: &
  mgv44_nb0(0:9) = mg431_nb, &
  mgv44_nb1(0:9) = mg431_nb, &
  mgv44_nb(0:9) = mgv44_nb0+mgv44_nb1, &
  mgv44_iord(0:nk-1,0:nk-1) = reshape( &
    (/4,5,6,7,1,2,3,0, &
      4,5,6,7,0,2,3,1, &
      4,5,6,7,0,1,3,2, &
      4,5,6,7,0,1,2,3, &
      0,1,2,3,5,6,7,4, &
      0,1,2,3,4,6,7,5, &
      0,1,2,3,4,5,7,6, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv44_base0.f90'
include 'mgv44_base1.f90'
include 'mgv44_base.f90'
END MODULE inv_mgv44
