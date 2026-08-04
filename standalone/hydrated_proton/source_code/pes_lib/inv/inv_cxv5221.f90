MODULE inv_cxv5221
!..use and access
use inv_core
use inv_mg5221
use inv_mgv5221
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b5221, cxv_f5221
!..data
integer, parameter, public :: &
  cxv_nb5221(-1:ubound(mgv5221_nb,dim=1))=(/0,mgv5221_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b5221.f90'
include 'cxv_f5221.f90'
END MODULE inv_cxv5221
