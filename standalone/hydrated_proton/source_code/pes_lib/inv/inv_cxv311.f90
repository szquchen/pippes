MODULE inv_cxv311
!..use and access
use inv_core
use inv_mg311
use inv_mgv311
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b311, cxv_f311, cxv_f131, cxv_f113, cxv_f3101, cxv_f3011
!..data
integer, parameter, public :: &
  cxv_nb311(-1:ubound(mgv311_nb,dim=1))=(/0,mgv311_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b311.f90'
include 'cxv_f311.f90'
include 'cxv_f131.f90'
include 'cxv_f113.f90'
include 'cxv_f3101.f90'
include 'cxv_f3011.f90'
END MODULE inv_cxv311
