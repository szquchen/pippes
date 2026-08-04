MODULE inv_mgv4
!..use and access
use inv_wp
use inv_mg4
use inv_mg31
implicit none
private
public :: mgv4_base0, mgv4_base
!..data
integer, parameter, private :: &
  nkk=mg4_nkk, nk=mg4_nk, nr=mg4_nr, &
  nkj(0:nkk-1)=mg4_nkj
integer, parameter, public :: &
  mgv4_nb0(0:19) = mg31_nb, &
  mgv4_nb(0:19) = mgv4_nb0, &
  mgv4_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,0, &
      0,2,3,1, &
      0,1,3,2, &
      0,1,2,3/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv4_base0.f90'
include 'mgv4_base.f90'
END MODULE inv_mgv4
