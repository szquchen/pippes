MODULE inv_cxv321111
!..use and access
use inv_core
use inv_mg321111
use inv_mgv321111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b321111, cxv_f321111
!..data
integer, parameter, public :: &
  cxv_nb321111(-1:ubound(mgv321111_nb,dim=1))=(/0,mgv321111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b321111.f90'
include 'cxv_f321111.f90'
END MODULE inv_cxv321111
