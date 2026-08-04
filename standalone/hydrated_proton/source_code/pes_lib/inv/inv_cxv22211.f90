MODULE inv_cxv22211
!..use and access
use inv_core
use inv_mg22211
use inv_mgv22211
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b22211, cxv_f22211
!..data
integer, parameter, public :: &
  cxv_nb22211(-1:ubound(mgv22211_nb,dim=1))=(/0,mgv22211_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b22211.f90'
include 'cxv_f22211.f90'
END MODULE inv_cxv22211
