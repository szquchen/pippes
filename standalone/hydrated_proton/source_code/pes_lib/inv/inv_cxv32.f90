MODULE inv_cxv32
!..use and access
use inv_core
use inv_mg32
use inv_mgv32
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b32, cxv_f32, cxv_f23, cxv_f302, cxv_f032, cxv_f203, cxv_f023
!..data
integer, parameter, public :: &
  cxv_nb32(-1:ubound(mgv32_nb,dim=1))=(/0,mgv32_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b32.f90'
include 'cxv_f32.f90'
include 'cxv_f23.f90'
include 'cxv_f302.f90'
include 'cxv_f032.f90'
include 'cxv_f203.f90'
include 'cxv_f023.f90'
END MODULE inv_cxv32
