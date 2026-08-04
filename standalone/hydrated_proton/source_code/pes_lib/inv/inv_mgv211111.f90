MODULE inv_mgv211111
!..use and access
use inv_wp
use inv_mg211111
use inv_mg1111111
implicit none
private
public :: mgv211111_base
!..data
integer, parameter, private :: &
  nkk=mg211111_nkk, nk=mg211111_nk, nr=mg211111_nr, &
  nkj(0:nkk-1)=mg211111_nkj
integer, parameter, public :: &
  mgv211111_nb0(0:9) = mg1111111_nb, &
  mgv211111_nb(0:9) = mgv211111_nb0+5*mg211111_nb, &
  mgv211111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,0,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv211111_base0.f90'
include 'mgv211111_base.f90'
END MODULE inv_mgv211111
