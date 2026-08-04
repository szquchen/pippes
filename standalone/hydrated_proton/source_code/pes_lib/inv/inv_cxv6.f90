MODULE inv_cxv6
!..use and access
use inv_core
use inv_mg6
use inv_mgv6
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b6, cxv_f6
!..data
integer, parameter, public :: &
  cxv_nb6(-1:ubound(mgv6_nb,dim=1))=(/0,mgv6_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b6.f90'
include 'cxv_f6.f90'
END MODULE inv_cxv6
