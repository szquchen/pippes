MODULE inv_mgv511
!..use and access
use inv_wp
use inv_mg511
use inv_mg4111
implicit none
private
public :: mgv511_base0, mgv511_base
!..data
integer, parameter, private :: &
  nkk=mg511_nkk, nk=mg511_nk, nr=mg511_nr, &
  nkj(0:nkk-1)=mg511_nkj
integer, parameter, public :: &
  mgv511_nb0(0:9) = mg4111_nb, &
  mgv511_nb(0:9) = mgv511_nb0+2*mg511_nb, &
  mgv511_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,0,5,6, &
      0,2,3,4,1,5,6, &
      0,1,3,4,2,5,6, &
      0,1,2,4,3,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6, &
      0,1,2,3,4,5,6/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv511_base0.f90'
include 'mgv511_base.f90'
END MODULE inv_mgv511
