MODULE inv_cxv2221111
!..use and access
use inv_core
use inv_mg2221111
use inv_mgv2221111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b2221111, cxv_f2221111
!..data
integer, parameter, public :: &
  cxv_nb2221111(-1:ubound(mgv2221111_nb,dim=1))=(/0,mgv2221111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b2221111.f90'
include 'cxv_f2221111.f90'
END MODULE inv_cxv2221111
