MODULE inv_cxv33
!..use and access
use inv_core
use inv_mg33
use inv_mgv33
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b33, cxv_f33, cxv_f303, cxv_f033
!..data
integer, parameter, public :: &
  cxv_nb33(-1:ubound(mgv33_nb,dim=1))=(/0,mgv33_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b33.f90'
include 'cxv_f33.f90'
include 'cxv_f303.f90'
include 'cxv_f033.f90'
END MODULE inv_cxv33
