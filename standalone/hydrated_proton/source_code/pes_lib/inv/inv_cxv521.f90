MODULE inv_cxv521
!..use and access
use inv_core
use inv_mg521
use inv_mgv521
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b521, cxv_f521
!..data
integer, parameter, public :: &
  cxv_nb521(-1:ubound(mgv521_nb,dim=1))=(/0,mgv521_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b521.f90'
include 'cxv_f521.f90'
END MODULE inv_cxv521
