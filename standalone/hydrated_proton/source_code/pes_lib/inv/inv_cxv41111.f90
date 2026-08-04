MODULE inv_cxv41111
!..use and access
use inv_core
use inv_mg41111
use inv_mgv41111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b41111, cxv_f41111
!..data
integer, parameter, public :: &
  cxv_nb41111(-1:ubound(mgv41111_nb,dim=1))=(/0,mgv41111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b41111.f90'
include 'cxv_f41111.f90'
END MODULE inv_cxv41111
