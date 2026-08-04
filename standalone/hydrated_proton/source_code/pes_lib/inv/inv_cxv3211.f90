MODULE inv_cxv3211
!..use and access
use inv_core
use inv_mg3211
use inv_mgv3211
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b3211, cxv_f3211
!..data
integer, parameter, public :: &
  cxv_nb3211(-1:ubound(mgv3211_nb,dim=1))=(/0,mgv3211_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b3211.f90'
include 'cxv_f3211.f90'
END MODULE inv_cxv3211
