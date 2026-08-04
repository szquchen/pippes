MODULE inv_cxv2221
!..use and access
use inv_core
use inv_mg2221
use inv_mgv2221
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b2221, cxv_f2221
!..data
integer, parameter, public :: &
  cxv_nb2221(-1:ubound(mgv2221_nb,dim=1))=(/0,mgv2221_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b2221.f90'
include 'cxv_f2221.f90'
END MODULE inv_cxv2221
