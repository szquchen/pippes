MODULE inv_cxv33111
!..use and access
use inv_core
use inv_mg33111
use inv_mgv33111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b33111, cxv_f33111
!..data
integer, parameter, public :: &
  cxv_nb33111(-1:ubound(mgv33111_nb,dim=1))=(/0,mgv33111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b33111.f90'
include 'cxv_f33111.f90'
END MODULE inv_cxv33111
