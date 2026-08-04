MODULE inv_mgv2
!..use and access
use inv_wp
use inv_mg2
use inv_mg11
implicit none
private
public :: mgv2_base0, mgv2_base
!..data
integer, parameter, private :: &
  nkk=mg2_nkk, nk=mg2_nk, nr=mg2_nr, &
  nkj(0:nkk-1)=mg2_nkj
integer, parameter, public :: &
  mgv2_nb0(0:19) = mg11_nb, &
  mgv2_nb(0:19) = mgv2_nb0, &
  mgv2_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,0, &
      0,1/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv2_base0.f90'
include 'mgv2_base.f90'
END MODULE inv_mgv2
