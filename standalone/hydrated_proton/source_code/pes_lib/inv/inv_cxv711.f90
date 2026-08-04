MODULE inv_cxv711
!..use and access
use inv_core
use inv_mg711
use inv_mgv711
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b711, cxv_f711
!..data
integer, parameter, public :: &
  cxv_nb711(-1:ubound(mgv711_nb,dim=1))=(/0,mgv711_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b711.f90'
include 'cxv_f711.f90'
END MODULE inv_cxv711
