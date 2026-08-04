MODULE inv_cxv4311
!..use and access
use inv_core
use inv_mg4311
use inv_mgv4311
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b4311, cxv_f4311
!..data
integer, parameter, public :: &
  cxv_nb4311(-1:ubound(mgv4311_nb,dim=1))=(/0,mgv4311_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b4311.f90'
include 'cxv_f4311.f90'
END MODULE inv_cxv4311
