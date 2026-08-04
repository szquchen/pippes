MODULE inv_cxv5
!..use and access
use inv_core
use inv_mg5
use inv_mgv5
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b5, cxv_f5
!..data
integer, parameter, public :: &
  cxv_nb5(-1:ubound(mgv5_nb,dim=1))=(/0,mgv5_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b5.f90'
include 'cxv_f5.f90'
END MODULE inv_cxv5
