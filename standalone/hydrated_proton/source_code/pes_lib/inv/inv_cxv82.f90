MODULE inv_cxv82
!..use and access
use inv_core
use inv_mg82
use inv_mgv82
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b82, cxv_f82
!..data
integer, parameter, public :: &
  cxv_nb82(-1:ubound(mgv82_nb,dim=1))=(/0,mgv82_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b82.f90'
include 'cxv_f82.f90'
END MODULE inv_cxv82
