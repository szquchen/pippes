MODULE inv_cxv22111
!..use and access
use inv_core
use inv_mg22111
use inv_mgv22111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b22111, cxv_f22111
!..data
integer, parameter, public :: &
  cxv_nb22111(-1:ubound(mgv22111_nb,dim=1))=(/0,mgv22111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b22111.f90'
include 'cxv_f22111.f90'
END MODULE inv_cxv22111
