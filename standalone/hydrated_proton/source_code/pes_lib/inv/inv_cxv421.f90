MODULE inv_cxv421
!..use and access
use inv_core
use inv_mg421
use inv_mgv421
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b421, cxv_f421, cxv_f412, cxv_f4201
!..data
integer, parameter, public :: &
  cxv_nb421(-1:ubound(mgv421_nb,dim=1))=(/0,mgv421_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b421.f90'
include 'cxv_f421.f90'
include 'cxv_f412.f90'
include 'cxv_f4201.f90'
END MODULE inv_cxv421
