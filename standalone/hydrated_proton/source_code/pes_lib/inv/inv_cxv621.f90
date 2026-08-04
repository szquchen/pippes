MODULE inv_cxv621
!..use and access
use inv_core
use inv_mg621
use inv_mgv621
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b621, cxv_f621
!..data
integer, parameter, public :: &
  cxv_nb621(-1:ubound(mgv621_nb,dim=1))=(/0,mgv621_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b621.f90'
include 'cxv_f621.f90'
END MODULE inv_cxv621
