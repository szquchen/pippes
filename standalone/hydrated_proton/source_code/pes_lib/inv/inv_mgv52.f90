MODULE inv_mgv52
!..use and access
use inv_wp
use inv_mg52
use inv_mg421
use inv_mg511
implicit none
private
public :: mgv52_base0, mgv52_base1, mgv52_base
!..data
integer, parameter, private :: &
  nkk=mg52_nkk, nk=mg52_nk, nr=mg52_nr, &
  nkj(0:nkk-1)=mg52_nkj
integer, parameter, public :: &
  mgv52_nb0(0:9) = mg421_nb, &
  mgv52_nb1(0:9) = mg511_nb, &
  mgv52_nb(0:9) = mgv52_nb0+mgv52_nb1, &
  mgv52_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,0, &
      0,2,3,4,5,6,1, &
      0,1,3,4,5,6,2, &
      0,1,2,4,5,6,3, &
      0,1,2,3,5,6,4, &
      0,1,2,3,4,6,5, &
      0,1,2,3,4,5,6/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv52_base0.f90'
include 'mgv52_base1.f90'
include 'mgv52_base.f90'
END MODULE inv_mgv52
