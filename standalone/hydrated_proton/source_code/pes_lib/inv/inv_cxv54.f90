MODULE inv_cxv54
!..use and access
use inv_core
use inv_mg54
use inv_mgv54
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b54, cxv_f54
!..data
integer, parameter, public :: &
  cxv_nb54(-1:ubound(mgv54_nb,dim=1))=(/0,mgv54_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b54.f90'
include 'cxv_f54.f90'
END MODULE inv_cxv54
