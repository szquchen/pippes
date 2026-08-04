MODULE inv_cxv2111111
!..use and access
use inv_core
use inv_mg2111111
use inv_mgv2111111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b2111111, cxv_f2111111
!..data
integer, parameter, public :: &
  cxv_nb2111111(-1:ubound(mgv2111111_nb,dim=1))=(/0,mgv2111111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b2111111.f90'
include 'cxv_f2111111.f90'
END MODULE inv_cxv2111111
