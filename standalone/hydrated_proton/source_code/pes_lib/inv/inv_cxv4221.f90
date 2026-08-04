MODULE inv_cxv4221
!..use and access
use inv_core
use inv_mg4221
use inv_mgv4221
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b4221, cxv_f4221
!..data
integer, parameter, public :: &
  cxv_nb4221(-1:ubound(mgv4221_nb,dim=1))=(/0,mgv4221_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b4221.f90'
include 'cxv_f4221.f90'
END MODULE inv_cxv4221
