MODULE inv_cxv73
!..use and access
use inv_core
use inv_mg73
use inv_mgv73
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b73, cxv_f73
!..data
integer, parameter, public :: &
  cxv_nb73(-1:ubound(mgv73_nb,dim=1))=(/0,mgv73_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b73.f90'
include 'cxv_f73.f90'
END MODULE inv_cxv73
