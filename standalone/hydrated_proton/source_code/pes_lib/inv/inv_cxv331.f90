MODULE inv_cxv331
!..use and access
use inv_core
use inv_mg331
use inv_mgv331
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b331, cxv_f331, cxv_f313, cxv_f133
!..data
integer, parameter, public :: &
  cxv_nb331(-1:ubound(mgv331_nb,dim=1))=(/0,mgv331_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b331.f90'
include 'cxv_f331.f90'
include 'cxv_f313.f90'
include 'cxv_f133.f90'
END MODULE inv_cxv331
