MODULE inv_cxv72
!..use and access
use inv_core
use inv_mg72
use inv_mgv72
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b72, cxv_f72
!..data
integer, parameter, public :: &
  cxv_nb72(-1:ubound(mgv72_nb,dim=1))=(/0,mgv72_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b72.f90'
include 'cxv_f72.f90'
END MODULE inv_cxv72
