MODULE inv_cxv21
!..use and access
use inv_core
use inv_mg21
use inv_mgv21
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b21, cxv_f21, cxv_f12, cxv_f201, cxv_f021, cxv_f102, &
  cxv_f012, cxv_f2001, cxv_f0201
!..data
integer, parameter, public :: &
  cxv_nb21(-1:ubound(mgv21_nb,dim=1))=(/0,mgv21_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b21.f90'
include 'cxv_f21.f90'
include 'cxv_f12.f90'
include 'cxv_f201.f90'
include 'cxv_f021.f90'
include 'cxv_f102.f90'
include 'cxv_f012.f90'
include 'cxv_f2001.f90'
include 'cxv_f0201.f90'
END MODULE inv_cxv21
