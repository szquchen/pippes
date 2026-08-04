MODULE inv_cxv2111
!..use and access
use inv_core
use inv_mg2111
use inv_mgv2111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b2111, cxv_f2111, cxv_f1211
!..data
integer, parameter, public :: &
  cxv_nb2111(-1:ubound(mgv2111_nb,dim=1))=(/0,mgv2111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b2111.f90'
include 'cxv_f2111.f90'
include 'cxv_f1211.f90'
END MODULE inv_cxv2111
