MODULE inv_cxv22111111
!..use and access
use inv_core
use inv_mg22111111
use inv_mgv22111111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b22111111, cxv_f22111111
!..data
integer, parameter, public :: &
  cxv_nb22111111(-1:ubound(mgv22111111_nb,dim=1))=(/0,mgv22111111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b22111111.f90'
include 'cxv_f22111111.f90'
END MODULE inv_cxv22111111
