MODULE inv_cxv631
!..use and access
use inv_core
use inv_mg631
use inv_mgv631
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b631, cxv_f631
!..data
integer, parameter, public :: &
  cxv_nb631(-1:ubound(mgv631_nb,dim=1))=(/0,mgv631_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b631.f90'
include 'cxv_f631.f90'
END MODULE inv_cxv631
