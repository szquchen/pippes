MODULE inv_cxv531
!..use and access
use inv_core
use inv_mg531
use inv_mgv531
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b531, cxv_f531
!..data
integer, parameter, public :: &
  cxv_nb531(-1:ubound(mgv531_nb,dim=1))=(/0,mgv531_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b531.f90'
include 'cxv_f531.f90'
END MODULE inv_cxv531
