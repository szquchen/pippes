MODULE inv_cxv5211
!..use and access
use inv_core
use inv_mg5211
use inv_mgv5211
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b5211, cxv_f5211
!..data
integer, parameter, public :: &
  cxv_nb5211(-1:ubound(mgv5211_nb,dim=1))=(/0,mgv5211_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b5211.f90'
include 'cxv_f5211.f90'
END MODULE inv_cxv5211
