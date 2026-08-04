MODULE inv_cxv31111111
!..use and access
use inv_core
use inv_mg31111111
use inv_mgv31111111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b31111111, cxv_f31111111
!..data
integer, parameter, public :: &
  cxv_nb31111111(-1:ubound(mgv31111111_nb,dim=1))=(/0,mgv31111111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b31111111.f90'
include 'cxv_f31111111.f90'
END MODULE inv_cxv31111111
