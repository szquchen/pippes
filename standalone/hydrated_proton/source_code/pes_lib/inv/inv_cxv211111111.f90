MODULE inv_cxv211111111
!..use and access
use inv_core
use inv_mg211111111
use inv_mgv211111111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b211111111, cxv_f211111111
!..data
integer, parameter, public :: &
  cxv_nb211111111(-1:ubound(mgv211111111_nb,dim=1))=(/0,mgv211111111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b211111111.f90'
include 'cxv_f211111111.f90'
END MODULE inv_cxv211111111
