MODULE inv_mgv333
!..use and access
use inv_wp
use inv_mg333
use inv_mg3321
implicit none
private
public :: mgv333_base0, mgv333_base1, mgv333_base2, mgv333_base
!..data
integer, parameter, private :: &
  nkk=mg333_nkk, nk=mg333_nk, nr=mg333_nr, &
  nkj(0:nkk-1)=mg333_nkj
integer, parameter, public :: &
  mgv333_nb0(0:9) = mg3321_nb, &
  mgv333_nb1(0:9) = mg3321_nb, &
  mgv333_nb2(0:9) = mg3321_nb, &
  mgv333_nb(0:9) = mgv333_nb0+mgv333_nb1+mgv333_nb2, &
  mgv333_iord(0:nk-1,0:nk-1) = reshape( &
    (/3,4,5,6,7,8,1,2,0, &
      3,4,5,6,7,8,0,2,1, &
      3,4,5,6,7,8,0,1,2, &
      0,1,2,6,7,8,4,5,3, &
      0,1,2,6,7,8,3,5,4, &
      0,1,2,6,7,8,3,4,5, &
      0,1,2,3,4,5,7,8,6, &
      0,1,2,3,4,5,6,8,7, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv333_base0.f90'
include 'mgv333_base1.f90'
include 'mgv333_base2.f90'
include 'mgv333_base.f90'
END MODULE inv_mgv333
