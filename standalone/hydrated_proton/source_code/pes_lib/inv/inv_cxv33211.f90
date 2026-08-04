MODULE inv_cxv33211
!..use and access
use inv_core
use inv_mg33211
use inv_mgv33211
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b33211, cxv_f33211
!..data
integer, parameter, public :: &
  cxv_nb33211(-1:ubound(mgv33211_nb,dim=1))=(/0,mgv33211_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b33211.f90'
include 'cxv_f33211.f90'
END MODULE inv_cxv33211
