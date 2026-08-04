MODULE inv_cxv43
!..use and access
use inv_core
use inv_mg43
use inv_mgv43
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b43, cxv_f43
!..data
integer, parameter, public :: &
  cxv_nb43(-1:ubound(mgv43_nb,dim=1))=(/0,mgv43_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b43.f90'
include 'cxv_f43.f90'
END MODULE inv_cxv43
