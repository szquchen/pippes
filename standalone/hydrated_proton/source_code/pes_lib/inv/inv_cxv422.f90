MODULE inv_cxv422
!..use and access
use inv_core
use inv_mg422
use inv_mgv422
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b422, cxv_f422
!..data
integer, parameter, public :: &
  cxv_nb422(-1:ubound(mgv422_nb,dim=1))=(/0,mgv422_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b422.f90'
include 'cxv_f422.f90'
END MODULE inv_cxv422
