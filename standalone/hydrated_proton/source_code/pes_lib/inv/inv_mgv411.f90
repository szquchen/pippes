MODULE inv_mgv411
!..use and access
use inv_wp
use inv_mg411
use inv_mg3111
implicit none
private
public :: mgv411_base0, mgv411_base
!..data
integer, parameter, private :: &
  nkk=mg411_nkk, nk=mg411_nk, nr=mg411_nr, &
  nkj(0:nkk-1)=mg411_nkj
integer, parameter, public :: &
  mgv411_nb0(0:9) = mg3111_nb, &
  mgv411_nb(0:9) = mgv411_nb0+2*mg411_nb, &
  mgv411_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,0,4,5, &
      0,2,3,1,4,5, &
      0,1,3,2,4,5, &
      0,1,2,3,4,5, &
      0,1,2,3,4,5, &
      0,1,2,3,4,5/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv411_base0.f90'
include 'mgv411_base.f90'
END MODULE inv_mgv411
