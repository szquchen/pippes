MODULE inv_cxv333
!..use and access
use inv_core
use inv_mg333
use inv_mgv333
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b333, cxv_f333
!..data
integer, parameter, public :: &
  cxv_nb333(-1:ubound(mgv333_nb,dim=1))=(/0,mgv333_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b333.f90'
include 'cxv_f333.f90'
END MODULE inv_cxv333
