MODULE inv_cxv7
!..use and access
use inv_core
use inv_mg7
use inv_mgv7
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b7, cxv_f7
!..data
integer, parameter, public :: &
  cxv_nb7(-1:ubound(mgv7_nb,dim=1))=(/0,mgv7_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b7.f90'
include 'cxv_f7.f90'
END MODULE inv_cxv7
