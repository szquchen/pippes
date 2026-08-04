MODULE inv_cxv11111
!..use and access
use inv_core
use inv_mg11111
use inv_mgv11111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b11111, cxv_f11111
!..data
integer, parameter, public :: &
  cxv_nb11111(-1:ubound(mgv11111_nb,dim=1))=(/0,mgv11111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b11111.f90'
include 'cxv_f11111.f90'
END MODULE inv_cxv11111
