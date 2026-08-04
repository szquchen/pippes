MODULE inv_cxv2
!..use and access
use inv_core
use inv_mg2
use inv_mgv2
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b2, cxv_f2, cxv_f02, cxv_f002
!..data
integer, parameter, public :: &
  cxv_nb2(-1:ubound(mgv2_nb,dim=1))=(/0,mgv2_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b2.f90'
include 'cxv_f2.f90'
include 'cxv_f02.f90'
include 'cxv_f002.f90'
END MODULE inv_cxv2
