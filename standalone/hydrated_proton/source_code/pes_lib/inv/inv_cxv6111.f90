MODULE inv_cxv6111
!..use and access
use inv_core
use inv_mg6111
use inv_mgv6111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b6111, cxv_f6111
!..data
integer, parameter, public :: &
  cxv_nb6111(-1:ubound(mgv6111_nb,dim=1))=(/0,mgv6111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b6111.f90'
include 'cxv_f6111.f90'
END MODULE inv_cxv6111
