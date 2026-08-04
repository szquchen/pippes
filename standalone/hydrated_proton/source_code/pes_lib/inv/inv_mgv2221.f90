MODULE inv_mgv2221
!..use and access
use inv_wp
use inv_mg2221
use inv_mg22111
implicit none
private
public :: mgv2221_base0, mgv2221_base1, mgv2221_base2, mgv2221_base
!..data
integer, parameter, private :: &
  nkk=mg2221_nkk, nk=mg2221_nk, nr=mg2221_nr, &
  nkj(0:nkk-1)=mg2221_nkj
integer, parameter, public :: &
  mgv2221_nb0(0:9) = mg22111_nb, &
  mgv2221_nb1(0:9) = mg22111_nb, &
  mgv2221_nb2(0:9) = mg22111_nb, &
  mgv2221_nb(0:9) = mgv2221_nb0+mgv2221_nb1+mgv2221_nb2+ &
    mg2221_nb, &
  mgv2221_iord(0:nk-1,0:nk-1) = reshape( &
    (/2,3,4,5,1,0,6, &
      2,3,4,5,0,1,6, &
      0,1,4,5,3,2,6, &
      0,1,4,5,2,3,6, &
      0,1,2,3,5,4,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv2221_base0.f90'
include 'mgv2221_base1.f90'
include 'mgv2221_base2.f90'
include 'mgv2221_base.f90'
END MODULE inv_mgv2221
