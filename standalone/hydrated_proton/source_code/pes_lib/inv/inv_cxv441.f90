MODULE inv_cxv441
!..use and access
use inv_core
use inv_mg441
use inv_mgv441
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b441, cxv_f441
!..data
integer, parameter, public :: &
  cxv_nb441(-1:ubound(mgv441_nb,dim=1))=(/0,mgv441_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b441.f90'
include 'cxv_f441.f90'
END MODULE inv_cxv441
