MODULE inv_mgv11
!..use and access
use inv_wp
use inv_mg11
implicit none
private
public :: mgv11_base
!..data
integer, parameter, private :: &
  nkk=mg11_nkk, nk=mg11_nk, nr=mg11_nr, &
  nkj(0:nkk-1)=mg11_nkj
integer, parameter, public :: &
  mgv11_nb(0:19) = 2*mg11_nb, &
  mgv11_iord(0:nk-1,0:nk-1) = reshape( &
    (/0,1, &
      0,1/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv11_base.f90'
END MODULE inv_mgv11
