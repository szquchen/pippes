MODULE inv_cxv111111
!..use and access
use inv_core
use inv_mg111111
use inv_mgv111111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b111111, cxv_f111111
!..data
integer, parameter, public :: &
  cxv_nb111111(-1:ubound(mgv111111_nb,dim=1))=(/0,mgv111111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b111111.f90'
include 'cxv_f111111.f90'
END MODULE inv_cxv111111
