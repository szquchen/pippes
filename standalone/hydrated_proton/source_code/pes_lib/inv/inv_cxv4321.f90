MODULE inv_cxv4321
!..use and access
use inv_core
use inv_mg4321
use inv_mgv4321
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b4321, cxv_f4321
!..data
integer, parameter, public :: &
  cxv_nb4321(-1:ubound(mgv4321_nb,dim=1))=(/0,mgv4321_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b4321.f90'
include 'cxv_f4321.f90'
END MODULE inv_cxv4321
