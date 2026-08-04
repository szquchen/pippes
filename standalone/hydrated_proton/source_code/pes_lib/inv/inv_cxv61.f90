MODULE inv_cxv61
!..use and access
use inv_core
use inv_mg61
use inv_mgv61
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b61, cxv_f61, cxv_f601, cxv_f6001
!..data
integer, parameter, public :: &
  cxv_nb61(-1:ubound(mgv61_nb,dim=1))=(/0,mgv61_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b61.f90'
include 'cxv_f61.f90'
include 'cxv_f601.f90'
include 'cxv_f6001.f90'
END MODULE inv_cxv61
