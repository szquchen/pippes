MODULE inv_mgv9
!..use and access
use inv_wp
use inv_mg9
use inv_mg81
implicit none
private
public :: mgv9_base0, mgv9_base
!..data
integer, parameter, private :: &
  nkk=mg9_nkk, nk=mg9_nk, nr=mg9_nr, &
  nkj(0:nkk-1)=mg9_nkj
END MODULE inv_mgv9
