MODULE inv_cxv52111
!..use and access
use inv_core
use inv_mg52111
use inv_mgv52111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b52111, cxv_f52111
!..data
integer, parameter, public :: &
  cxv_nb52111(-1:ubound(mgv52111_nb,dim=1))=(/0,mgv52111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b52111.f90'
include 'cxv_f52111.f90'
END MODULE inv_cxv52111
