MODULE inv_cxv22
!..use and access
use inv_core
use inv_mg22
use inv_mgv22
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b22, cxv_f22, cxv_f202, cxv_f022
!..data
integer, parameter, public :: &
  cxv_nb22(-1:ubound(mgv22_nb,dim=1))=(/0,mgv22_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b22.f90'
include 'cxv_f22.f90'
include 'cxv_f202.f90'
include 'cxv_f022.f90'
END MODULE inv_cxv22
