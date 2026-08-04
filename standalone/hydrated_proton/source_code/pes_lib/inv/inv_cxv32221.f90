MODULE inv_cxv32221
!..use and access
use inv_core
use inv_mg32221
use inv_mgv32221
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b32221, cxv_f32221
!..data
integer, parameter, public :: &
  cxv_nb32221(-1:ubound(mgv32221_nb,dim=1))=(/0,mgv32221_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b32221.f90'
include 'cxv_f32221.f90'
END MODULE inv_cxv32221
