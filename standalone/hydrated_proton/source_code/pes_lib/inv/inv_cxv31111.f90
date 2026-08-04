MODULE inv_cxv31111
!..use and access
use inv_core
use inv_mg31111
use inv_mgv31111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b31111, cxv_f31111
!..data
integer, parameter, public :: &
  cxv_nb31111(-1:ubound(mgv31111_nb,dim=1))=(/0,mgv31111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b31111.f90'
include 'cxv_f31111.f90'
END MODULE inv_cxv31111
