MODULE inv_cxv52
!..use and access
use inv_core
use inv_mg52
use inv_mgv52
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b52, cxv_f52
!..data
integer, parameter, public :: &
  cxv_nb52(-1:ubound(mgv52_nb,dim=1))=(/0,mgv52_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b52.f90'
include 'cxv_f52.f90'
END MODULE inv_cxv52
