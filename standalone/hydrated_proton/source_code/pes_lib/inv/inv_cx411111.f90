MODULE inv_cx411111
!..use and access
use inv_core
use inv_mg411111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b411111, cx_f411111
!..data
integer, parameter, public :: &
  cx_nb411111(-1:ubound(mg411111_nb,dim=1))=(/0,mg411111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b411111.f90'
include 'cx_f411111.f90'
END MODULE inv_cx411111
