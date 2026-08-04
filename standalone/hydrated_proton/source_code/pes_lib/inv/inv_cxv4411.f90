MODULE inv_cxv4411
!..use and access
use inv_core
use inv_mg4411
use inv_mgv4411
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b4411, cxv_f4411
!..data
integer, parameter, public :: &
  cxv_nb4411(-1:ubound(mgv4411_nb,dim=1))=(/0,mgv4411_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b4411.f90'
include 'cxv_f4411.f90'
END MODULE inv_cxv4411
