MODULE inv_mgv51
!..use and access
use inv_wp
use inv_mg51
use inv_mg411
implicit none
private
public :: mgv51_base0, mgv51_base
!..data
integer, parameter, private :: &
  nkk=mg51_nkk, nk=mg51_nk, nr=mg51_nr, &
  nkj(0:nkk-1)=mg51_nkj
integer, parameter, public :: &
  mgv51_nb0(0:9) = mg411_nb, &
  mgv51_nb(0:9) = mgv51_nb0+mg51_nb, &
  mgv51_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,0,5, &
      0,2,3,4,1,5, &
      0,1,3,4,2,5, &
      0,1,2,4,3,5, &
      0,1,2,3,4,5, &
      0,1,2,3,4,5/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv51_base0.f90'
include 'mgv51_base.f90'
END MODULE inv_mgv51
