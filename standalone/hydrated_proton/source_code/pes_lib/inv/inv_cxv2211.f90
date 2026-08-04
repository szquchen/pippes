MODULE inv_cxv2211
!..use and access
use inv_core
use inv_mg2211
use inv_mgv2211
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b2211, cxv_f2211
!..data
integer, parameter, public :: &
  cxv_nb2211(-1:ubound(mgv2211_nb,dim=1))=(/0,mgv2211_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b2211.f90'
include 'cxv_f2211.f90'
END MODULE inv_cxv2211
