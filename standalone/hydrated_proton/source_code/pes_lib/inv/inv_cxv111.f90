MODULE inv_cxv111
!..use and access
use inv_core
use inv_mg111
use inv_mgv111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b111, cxv_f111, cxv_f1101, cxv_f1011, cxv_f0111
!..data
integer, parameter, public :: &
  cxv_nb111(-1:ubound(mgv111_nb,dim=1))=(/0,mgv111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b111.f90'
include 'cxv_f111.f90'
include 'cxv_f1101.f90'
include 'cxv_f1011.f90'
include 'cxv_f0111.f90'
END MODULE inv_cxv111
