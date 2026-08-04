MODULE inv_mgv221
!..use and access
use inv_wp
use inv_mg221
use inv_mg2111
implicit none
private
public :: mgv221_base0, mgv221_base1, mgv221_base
!..data
integer, parameter, private :: &
  nkk=mg221_nkk, nk=mg221_nk, nr=mg221_nr, &
  nkj(0:nkk-1)=mg221_nkj
integer, parameter, public :: &
  mgv221_nb0(0:9) = mg2111_nb, &
  mgv221_nb1(0:9) = mg2111_nb, &
  mgv221_nb(0:9) = mgv221_nb0+mgv221_nb1+mg221_nb, &
  mgv221_iord(0:nk-1,0:nk-1) = reshape( &
    (/2,3,1,0,4, &
      2,3,0,1,4, &
      0,1,3,2,4, &
      0,1,2,3,4, &
      0,1,2,3,4/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv221_base0.f90'
include 'mgv221_base1.f90'
include 'mgv221_base.f90'
END MODULE inv_mgv221
