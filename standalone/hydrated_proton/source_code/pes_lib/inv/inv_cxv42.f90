MODULE inv_cxv42
!..use and access
use inv_core
use inv_mg42
use inv_mgv42
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b42, cxv_f42, cxv_f24, cxv_f402, cxv_f042, cxv_f204, cxv_f024
!..data
integer, parameter, public :: &
  cxv_nb42(-1:ubound(mgv42_nb,dim=1))=(/0,mgv42_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b42.f90'
include 'cxv_f42.f90'
include 'cxv_f24.f90'
include 'cxv_f402.f90'
include 'cxv_f042.f90'
include 'cxv_f204.f90'
include 'cxv_f024.f90'
END MODULE inv_cxv42
