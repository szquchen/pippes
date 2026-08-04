MODULE inv_cxv4111111
!..use and access
use inv_core
use inv_mg4111111
use inv_mgv4111111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b4111111, cxv_f4111111
!..data
integer, parameter, public :: &
  cxv_nb4111111(-1:ubound(mgv4111111_nb,dim=1))=(/0,mgv4111111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b4111111.f90'
include 'cxv_f4111111.f90'
END MODULE inv_cxv4111111
