MODULE inv_cxv2222
!..use and access
use inv_core
use inv_mg2222
use inv_mgv2222
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b2222, cxv_f2222
!..data
integer, parameter, public :: &
  cxv_nb2222(-1:ubound(mgv2222_nb,dim=1))=(/0,mgv2222_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b2222.f90'
include 'cxv_f2222.f90'
END MODULE inv_cxv2222
