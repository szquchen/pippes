MODULE inv_cxv4
!..use and access
use inv_core
use inv_mg4
use inv_mgv4
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b4, cxv_f4, cxv_f04, cxv_f004
!..data
integer, parameter, public :: &
  cxv_nb4(-1:ubound(mgv4_nb,dim=1))=(/0,mgv4_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b4.f90'
include 'cxv_f4.f90'
include 'cxv_f04.f90'
include 'cxv_f004.f90'
END MODULE inv_cxv4
