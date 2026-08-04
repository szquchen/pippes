MODULE inv_cxv3322
!..use and access
use inv_core
use inv_mg3322
use inv_mgv3322
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b3322, cxv_f3322
!..data
integer, parameter, public :: &
  cxv_nb3322(-1:ubound(mgv3322_nb,dim=1))=(/0,mgv3322_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b3322.f90'
include 'cxv_f3322.f90'
END MODULE inv_cxv3322
