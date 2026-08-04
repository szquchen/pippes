MODULE inv_mgv511111
!..use and access
use inv_wp
use inv_mg511111
use inv_mg4111111
implicit none
private
public :: mgv511111_base0, mgv511111_base
!..data
integer, parameter, private :: &
  nkk=mg511111_nkk, nk=mg511111_nk, nr=mg511111_nr, &
  nkj(0:nkk-1)=mg511111_nkj
integer, parameter, public :: &
  mgv511111_nb0(0:9) = mg4111111_nb, &
  mgv511111_nb(0:9) = mgv511111_nb0+5*mg511111_nb, &
  mgv511111_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,0,5,6,7,8,9, &
      0,2,3,4,1,5,6,7,8,9, &
      0,1,3,4,2,5,6,7,8,9, &
      0,1,2,4,3,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv511111_base0.f90'
include 'mgv511111_base.f90'
END MODULE inv_mgv511111
