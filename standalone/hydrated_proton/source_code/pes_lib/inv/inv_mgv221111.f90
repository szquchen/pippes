MODULE inv_mgv221111
!..use and access
use inv_wp
use inv_mg221111
use inv_mg2111111
implicit none
private
public :: mgv221111_base0, mgv221111_base1, &
  mgv221111_base
!..data
integer, parameter, private :: &
  nkk=mg221111_nkk, nk=mg221111_nk, nr=mg221111_nr, &
  nkj(0:nkk-1)=mg221111_nkj
integer, parameter, public :: &
  mgv221111_nb0(0:9) = mg2111111_nb, &
  mgv221111_nb1(0:9) = mg2111111_nb, &
  mgv221111_nb(0:9) = mgv221111_nb0+mgv221111_nb1+4*mg221111_nb, &
  mgv221111_iord(0:nk-1,0:nk-1) = reshape( &
    (/2,3,1,0,4,5,6,7, &
      2,3,0,1,4,5,6,7, &
      0,1,3,2,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7, &
      0,1,2,3,4,5,6,7/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv221111_base0.f90'
include 'mgv221111_base1.f90'
include 'mgv221111_base.f90'
END MODULE inv_mgv221111
