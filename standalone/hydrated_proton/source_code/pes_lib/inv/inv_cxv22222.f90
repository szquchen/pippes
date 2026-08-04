MODULE inv_cxv22222
!..use and access
use inv_core
use inv_mg22222
use inv_mgv22222
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b22222, cxv_f22222
!..data
integer, parameter, public :: &
  cxv_nb22222(-1:ubound(mgv22222_nb,dim=1))=(/0,mgv22222_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b22222.f90'
include 'cxv_f22222.f90'
END MODULE inv_cxv22222
