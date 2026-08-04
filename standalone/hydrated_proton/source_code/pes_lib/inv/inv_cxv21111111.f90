MODULE inv_cxv21111111
!..use and access
use inv_core
use inv_mg21111111
use inv_mgv21111111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b21111111, cxv_f21111111
!..data
integer, parameter, public :: &
  cxv_nb21111111(-1:ubound(mgv21111111_nb,dim=1))=(/0,mgv21111111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b21111111.f90'
include 'cxv_f21111111.f90'
END MODULE inv_cxv21111111
