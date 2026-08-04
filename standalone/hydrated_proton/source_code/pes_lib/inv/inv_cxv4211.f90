MODULE inv_cxv4211
!..use and access
use inv_core
use inv_mg4211
use inv_mgv4211
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b4211, cxv_f4211
!..data
integer, parameter, public :: &
  cxv_nb4211(-1:ubound(mgv4211_nb,dim=1))=(/0,mgv4211_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b4211.f90'
include 'cxv_f4211.f90'
END MODULE inv_cxv4211
