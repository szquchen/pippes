MODULE inv_mgv52111
!..use and access
use inv_wp
use inv_mg52111
use inv_mg511111
use inv_mg421111
implicit none
private
public :: mgv52111_base0, mgv52111_base1, mgv52111_base
!..data
integer, parameter, private :: &
  nkk=mg52111_nkk, nk=mg52111_nk, nr=mg52111_nr, &
  nkj(0:nkk-1)=mg52111_nkj
integer, parameter, public :: &
  mgv52111_nb0(0:9) = mg421111_nb, &
  mgv52111_nb1(0:9) = mg511111_nb, &
  mgv52111_nb(0:9) = mgv52111_nb0+mgv52111_nb1+3*mg52111_nb, &
  mgv52111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,0,7,8,9, &
      0,2,3,4,5,6,1,7,8,9, &
      0,1,3,4,5,6,2,7,8,9, &
      0,1,2,4,5,6,3,7,8,9, &
      0,1,2,3,5,6,4,7,8,9, &
      0,1,2,3,4,6,5,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv52111_base0.f90'
include 'mgv52111_base1.f90'
include 'mgv52111_base.f90'
END MODULE inv_mgv52111
