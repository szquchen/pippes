MODULE inv_cxv71
!..use and access
use inv_core
use inv_mg71
use inv_mgv71
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b71, cxv_f71
!..data
integer, parameter, public :: &
  cxv_nb71(-1:ubound(mgv71_nb,dim=1))=(/0,mgv71_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b71.f90'
include 'cxv_f71.f90'
END MODULE inv_cxv71
