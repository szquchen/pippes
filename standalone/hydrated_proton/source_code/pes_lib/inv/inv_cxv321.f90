MODULE inv_cxv321
!..use and access
use inv_core
use inv_mg321
use inv_mgv321
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b321, cxv_f321, cxv_f231, cxv_f312, cxv_f132, &
  cxv_f213, cxv_f123, cxv_f3201
!..data
integer, parameter, public :: &
  cxv_nb321(-1:ubound(mgv321_nb,dim=1))=(/0,mgv321_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b321.f90'
include 'cxv_f321.f90'
include 'cxv_f231.f90'
include 'cxv_f312.f90'
include 'cxv_f132.f90'
include 'cxv_f213.f90'
include 'cxv_f123.f90'
include 'cxv_f3201.f90'
END MODULE inv_cxv321
