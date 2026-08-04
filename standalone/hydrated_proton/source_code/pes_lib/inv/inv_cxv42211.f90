MODULE inv_cxv42211
!..use and access
use inv_core
use inv_mg42211
use inv_mgv42211
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b42211, cxv_f42211
!..data
integer, parameter, public :: &
  cxv_nb42211(-1:ubound(mgv42211_nb,dim=1))=(/0,mgv42211_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b42211.f90'
include 'cxv_f42211.f90'
END MODULE inv_cxv42211
