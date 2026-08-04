MODULE inv_cxv32111
!..use and access
use inv_core
use inv_mg32111
use inv_mgv32111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b32111, cxv_f32111
!..data
integer, parameter, public :: &
  cxv_nb32111(-1:ubound(mgv32111_nb,dim=1))=(/0,mgv32111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b32111.f90'
include 'cxv_f32111.f90'
END MODULE inv_cxv32111
