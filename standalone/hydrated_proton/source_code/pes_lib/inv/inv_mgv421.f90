MODULE inv_mgv421
!..use and access
use inv_wp
use inv_mg421
use inv_mg3211
use inv_mg4111
implicit none
private
public :: mgv421_base0, mgv421_base1, mgv421_base
!..data
integer, parameter, private :: &
  nkk=mg421_nkk, nk=mg421_nk, nr=mg421_nr, &
  nkj(0:nkk-1)=mg421_nkj
integer, parameter, public :: &
  mgv421_nb0(0:9) = mg3211_nb, &
  mgv421_nb1(0:9) = mg4111_nb, &
  mgv421_nb(0:9) = mgv421_nb0+mgv421_nb1+mg421_nb, &
  mgv421_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,0,6, &
      0,2,3,4,5,1,6, &
      0,1,3,4,5,2,6, &
      0,1,2,4,5,3,6, &
      0,1,2,3,5,4,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv421_base0.f90'
include 'mgv421_base1.f90'
include 'mgv421_base.f90'
END MODULE inv_mgv421
