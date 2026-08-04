MODULE inv_cxv211111
!..use and access
use inv_core
use inv_mg211111
use inv_mgv211111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b211111, cxv_f211111
!..data
integer, parameter, public :: &
  cxv_nb211111(-1:ubound(mgv211111_nb,dim=1))=(/0,mgv211111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b211111.f90'
include 'cxv_f211111.f90'
END MODULE inv_cxv211111
