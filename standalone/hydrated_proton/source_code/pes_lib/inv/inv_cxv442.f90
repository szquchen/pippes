MODULE inv_cxv442
!..use and access
use inv_core
use inv_mg442
use inv_mgv442
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b442, cxv_f442
!..data
integer, parameter, public :: &
  cxv_nb442(-1:ubound(mgv442_nb,dim=1))=(/0,mgv442_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b442.f90'
include 'cxv_f442.f90'
END MODULE inv_cxv442
