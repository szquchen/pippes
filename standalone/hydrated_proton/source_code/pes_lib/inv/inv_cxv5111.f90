MODULE inv_cxv5111
!..use and access
use inv_core
use inv_mg5111
use inv_mgv5111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b5111, cxv_f5111
!..data
integer, parameter, public :: &
  cxv_nb5111(-1:ubound(mgv5111_nb,dim=1))=(/0,mgv5111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b5111.f90'
include 'cxv_f5111.f90'
END MODULE inv_cxv5111
