MODULE inv_cxv22221
!..use and access
use inv_core
use inv_mg22221
use inv_mgv22221
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b22221, cxv_f22221
!..data
integer, parameter, public :: &
  cxv_nb22221(-1:ubound(mgv22221_nb,dim=1))=(/0,mgv22221_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b22221.f90'
include 'cxv_f22221.f90'
END MODULE inv_cxv22221
