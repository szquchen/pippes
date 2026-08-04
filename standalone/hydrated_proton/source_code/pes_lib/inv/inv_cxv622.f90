MODULE inv_cxv622
!..use and access
use inv_core
use inv_mg622
use inv_mgv622
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b622, cxv_f622
!..data
integer, parameter, public :: &
  cxv_nb622(-1:ubound(mgv622_nb,dim=1))=(/0,mgv622_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b622.f90'
include 'cxv_f622.f90'
END MODULE inv_cxv622
