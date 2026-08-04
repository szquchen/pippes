MODULE inv_cxv11
!..use and access
use inv_core
use inv_mg11
use inv_mgv11
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b11, cxv_f11, cxv_f101, cxv_f011, cxv_f1001, cxv_f0101, cxv_f0011
!..data
integer, parameter, public :: &
  cxv_nb11(-1:ubound(mgv11_nb,dim=1))=(/0,mgv11_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b11.f90'
include 'cxv_f11.f90'
include 'cxv_f101.f90'
include 'cxv_f011.f90'
include 'cxv_f1001.f90'
include 'cxv_f0101.f90'
include 'cxv_f0011.f90'
END MODULE inv_cxv11
