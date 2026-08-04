MODULE inv_cxv421111
!..use and access
use inv_core
use inv_mg421111
use inv_mgv421111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b421111, cxv_f421111
!..data
integer, parameter, public :: &
  cxv_nb421111(-1:ubound(mgv421111_nb,dim=1))=(/0,mgv421111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b421111.f90'
include 'cxv_f421111.f90'
END MODULE inv_cxv421111
