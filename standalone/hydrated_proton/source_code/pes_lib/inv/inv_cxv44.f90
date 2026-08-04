MODULE inv_cxv44
!..use and access
use inv_core
use inv_mg44
use inv_mgv44
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b44, cxv_f44
!..data
integer, parameter, public :: &
  cxv_nb44(-1:ubound(mgv44_nb,dim=1))=(/0,mgv44_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b44.f90'
include 'cxv_f44.f90'
END MODULE inv_cxv44
