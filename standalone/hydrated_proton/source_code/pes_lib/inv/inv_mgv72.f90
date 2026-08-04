MODULE inv_mgv72
!..use and access
use inv_wp
use inv_mg72
use inv_mg711
use inv_mg621
implicit none
private
public :: mgv72_base0, mgv72_base1, mgv72_base
!..data
integer, parameter, private :: &
  nkk=mg72_nkk, nk=mg72_nk, nr=mg72_nr, &
  nkj(0:nkk-1)=mg72_nkj
integer, parameter, public :: &
  mgv72_nb0(0:9) = mg621_nb, &
  mgv72_nb1(0:9) = mg711_nb, &
  mgv72_nb(0:9) = mgv72_nb0+mgv72_nb1, &
  mgv72_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,8,0, &
      0,2,3,4,5,6,7,8,1, &
      0,1,3,4,5,6,7,8,2, &
      0,1,2,4,5,6,7,8,3, &
      0,1,2,3,5,6,7,8,4, &
      0,1,2,3,4,6,7,8,5, &
      0,1,2,3,4,5,7,8,6, &
      0,1,2,3,4,5,6,8,7, &
      0,1,2,3,4,5,6,7,8/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv72_base0.f90'
include 'mgv72_base1.f90'
include 'mgv72_base.f90'
END MODULE inv_mgv72
