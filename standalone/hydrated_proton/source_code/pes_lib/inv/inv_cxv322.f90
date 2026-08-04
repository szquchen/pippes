MODULE inv_cxv322
!..use and access
use inv_core
use inv_mg322
use inv_mgv322
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b322, cxv_f322, cxv_f232, cxv_f223
!..data
integer, parameter, public :: &
  cxv_nb322(-1:ubound(mgv322_nb,dim=1))=(/0,mgv322_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b322.f90'
include 'cxv_f322.f90'
include 'cxv_f232.f90'
include 'cxv_f223.f90'
END MODULE inv_cxv322
