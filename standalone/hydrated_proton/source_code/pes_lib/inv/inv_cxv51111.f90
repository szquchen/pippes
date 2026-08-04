MODULE inv_cxv51111
!..use and access
use inv_core
use inv_mg51111
use inv_mgv51111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b51111, cxv_f51111
!..data
integer, parameter, public :: &
  cxv_nb51111(-1:ubound(mgv51111_nb,dim=1))=(/0,mgv51111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b51111.f90'
include 'cxv_f51111.f90'
END MODULE inv_cxv51111
