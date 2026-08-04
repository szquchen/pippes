MODULE inv_cxv432
!..use and access
use inv_core
use inv_mg432
use inv_mgv432
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b432, cxv_f432
!..data
integer, parameter, public :: &
  cxv_nb432(-1:ubound(mgv432_nb,dim=1))=(/0,mgv432_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b432.f90'
include 'cxv_f432.f90'
END MODULE inv_cxv432
