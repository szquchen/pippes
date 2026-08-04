MODULE inv_cxv61111
!..use and access
use inv_core
use inv_mg61111
use inv_mgv61111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b61111, cxv_f61111
!..data
integer, parameter, public :: &
  cxv_nb61111(-1:ubound(mgv61111_nb,dim=1))=(/0,mgv61111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b61111.f90'
include 'cxv_f61111.f90'
END MODULE inv_cxv61111
