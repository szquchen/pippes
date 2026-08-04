MODULE inv_cxv6211
!..use and access
use inv_core
use inv_mg6211
use inv_mgv6211
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b6211, cxv_f6211
!..data
integer, parameter, public :: &
  cxv_nb6211(-1:ubound(mgv6211_nb,dim=1))=(/0,mgv6211_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b6211.f90'
include 'cxv_f6211.f90'
END MODULE inv_cxv6211
