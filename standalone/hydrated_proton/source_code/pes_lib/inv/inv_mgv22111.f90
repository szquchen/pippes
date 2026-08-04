MODULE inv_mgv22111
!..use and access
use inv_wp
use inv_mg22111
use inv_mg211111
implicit none
private
public :: mgv22111_base0, mgv22111_base1, mgv22111_base
!..data
integer, parameter, private :: &
  nkk=mg22111_nkk, nk=mg22111_nk, nr=mg22111_nr, &
  nkj(0:nkk-1)=mg22111_nkj
integer, parameter, public :: &
  mgv22111_nb0(0:9) = mg211111_nb, &
  mgv22111_nb1(0:9) = mg211111_nb, &
  mgv22111_nb(0:9) = mgv22111_nb0+mgv22111_nb1+3*mg22111_nb, &
  mgv22111_iord(0:nk-1,0:nk-1) = reshape( &
    (/2,3,1,0,4,5,6, &
      2,3,0,1,4,5,6, &
      0,1,3,2,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv22111_base0.f90'
include 'mgv22111_base1.f90'
include 'mgv22111_base.f90'
END MODULE inv_mgv22111
