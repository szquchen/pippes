MODULE inv_cxv63
!..use and access
use inv_core
use inv_mg63
use inv_mgv63
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b63, cxv_f63
!..data
integer, parameter, public :: &
  cxv_nb63(-1:ubound(mgv63_nb,dim=1))=(/0,mgv63_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b63.f90'
include 'cxv_f63.f90'
END MODULE inv_cxv63
