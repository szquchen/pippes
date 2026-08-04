MODULE inv_cxv331111
!..use and access
use inv_core
use inv_mg331111
use inv_mgv331111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b331111, cxv_f331111
!..data
integer, parameter, public :: &
  cxv_nb331111(-1:ubound(mgv331111_nb,dim=1))=(/0,mgv331111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b331111.f90'
include 'cxv_f331111.f90'
END MODULE inv_cxv331111
