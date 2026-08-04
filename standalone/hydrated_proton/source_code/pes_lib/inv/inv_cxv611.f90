MODULE inv_cxv611
!..use and access
use inv_core
use inv_mg611
use inv_mgv611
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b611, cxv_f611, cxv_f6101, cxv_f6011
!..data
integer, parameter, public :: &
  cxv_nb611(-1:ubound(mgv611_nb,dim=1))=(/0,mgv611_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b611.f90'
include 'cxv_f611.f90'
include 'cxv_f6101.f90'
include 'cxv_f6011.f90'
END MODULE inv_cxv611
