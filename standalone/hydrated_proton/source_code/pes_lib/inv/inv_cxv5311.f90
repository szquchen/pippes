MODULE inv_cxv5311
!..use and access
use inv_core
use inv_mg5311
use inv_mgv5311
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b5311, cxv_f5311
!..data
integer, parameter, public :: &
  cxv_nb5311(-1:ubound(mgv5311_nb,dim=1))=(/0,mgv5311_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b5311.f90'
include 'cxv_f5311.f90'
END MODULE inv_cxv5311
