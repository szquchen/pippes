MODULE inv_mgv2221111
!..use and access
use inv_wp
use inv_mg2221111
use inv_mg22111111
implicit none
private
public :: mgv2221111_base0, mgv2221111_base1, mgv2221111_base2, &
  mgv2221111_base
!..data
integer, parameter, private :: &
  nkk=mg2221111_nkk, nk=mg2221111_nk, nr=mg2221111_nr, &
  nkj(0:nkk-1)=mg2221111_nkj
integer, parameter, public :: &
  mgv2221111_nb0(0:8) = mg22111111_nb(0:8), &
  mgv2221111_nb1(0:8) = mg22111111_nb(0:8), &
  mgv2221111_nb2(0:8) = mg22111111_nb(0:8), &
  mgv2221111_nb(0:8) = mgv2221111_nb0+mgv2221111_nb1+mgv2221111_nb2+ &
    3*mg2221111_nb(0:8), &
  mgv2221111_iord(0:nk-1,0:nk-1) = reshape( &
    (/2,3,4,5,1,0,6,7,8,9, &
      2,3,4,5,0,1,6,7,8,9, &
      0,1,4,5,3,2,6,7,8,9, &
      0,1,4,5,2,3,6,7,8,9, &
      0,1,2,3,5,4,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv2221111_base0.f90'
include 'mgv2221111_base1.f90'
include 'mgv2221111_base2.f90'
include 'mgv2221111_base.f90'
END MODULE inv_mgv2221111
