MODULE inv_mgv6
!..use and access
use inv_wp
use inv_mg6
use inv_mg51
implicit none
private
public :: mgv6_base0, mgv6_base
!..data
integer, parameter, private :: &
  nkk=mg6_nkk, nk=mg6_nk, nr=mg6_nr, &
  nkj(0:nkk-1)=mg6_nkj
integer, parameter, public :: &
  mgv6_nb0(0:9) = mg51_nb, &
  mgv6_nb(0:9) = mgv6_nb0, &
  mgv6_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,0, &
      0,2,3,4,5,1, &
      0,1,3,4,5,2, &
      0,1,2,4,5,3, &
      0,1,2,3,5,4, &
      0,1,2,3,4,5/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv6_base0.f90'
include 'mgv6_base.f90'
END MODULE inv_mgv6
