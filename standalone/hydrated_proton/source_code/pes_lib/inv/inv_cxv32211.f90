MODULE inv_cxv32211
!..use and access
use inv_core
use inv_mg32211
use inv_mgv32211
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b32211, cxv_f32211
!..data
integer, parameter, public :: &
  cxv_nb32211(-1:ubound(mgv32211_nb,dim=1))=(/0,mgv32211_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b32211.f90'
include 'cxv_f32211.f90'
END MODULE inv_cxv32211
