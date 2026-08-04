MODULE inv_cxv411111
!..use and access
use inv_core
use inv_mg411111
use inv_mgv411111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b411111, cxv_f411111
!..data
integer, parameter, public :: &
  cxv_nb411111(-1:ubound(mgv411111_nb,dim=1))=(/0,mgv411111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b411111.f90'
include 'cxv_f411111.f90'
END MODULE inv_cxv411111
