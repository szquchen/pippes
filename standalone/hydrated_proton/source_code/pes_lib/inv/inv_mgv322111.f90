MODULE inv_mgv322111
!..use and access
use inv_wp
use inv_mg322111
use inv_mg3211111
use inv_mg2221111
implicit none
private
public :: mgv322111_base0, mgv322111_base1, mgv322111_base2, mgv322111_base
!..data
integer, parameter, private :: &
  nkk=mg322111_nkk, nk=mg322111_nk, nr=mg322111_nr, &
  nkj(0:nkk-1)=mg322111_nkj
integer, parameter, public :: &
  mgv322111_nb0(0:8) = mg2221111_nb(0:8), &
  mgv322111_nb1(0:8) = mg3211111_nb(0:8), &
  mgv322111_nb2(0:8) = mg3211111_nb(0:8), &
  mgv322111_nb(0:8) = mgv322111_nb0+mgv322111_nb1+mgv322111_nb2+ &
    3*mg322111_nb(0:8), &
  mgv322111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,0,7,8,9, &
      0,2,3,4,5,6,1,7,8,9, &
      0,1,3,4,5,6,2,7,8,9, &
      0,1,2,5,6,4,3,7,8,9, &
      0,1,2,5,6,3,4,7,8,9, &
      0,1,2,3,4,6,5,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv322111_base0.f90'
include 'mgv322111_base1.f90'
include 'mgv322111_base2.f90'
include 'mgv322111_base.f90'
END MODULE inv_mgv322111
