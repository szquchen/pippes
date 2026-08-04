MODULE inv_cxv42111
!..use and access
use inv_core
use inv_mg42111
use inv_mgv42111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b42111, cxv_f42111
!..data
integer, parameter, public :: &
  cxv_nb42111(-1:ubound(mgv42111_nb,dim=1))=(/0,mgv42111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b42111.f90'
include 'cxv_f42111.f90'
END MODULE inv_cxv42111
