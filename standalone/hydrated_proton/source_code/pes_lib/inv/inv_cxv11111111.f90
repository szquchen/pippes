MODULE inv_cxv11111111
!..use and access
use inv_core
use inv_mg11111111
use inv_mgv11111111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b11111111, cxv_f11111111
!..data
integer, parameter, public :: &
  cxv_nb11111111(-1:ubound(mgv11111111_nb,dim=1))=(/0,mgv11111111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b11111111.f90'
include 'cxv_f11111111.f90'
END MODULE inv_cxv11111111
