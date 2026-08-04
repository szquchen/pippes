MODULE inv_cxv411
!..use and access
use inv_core
use inv_mg411
use inv_mgv411
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b411, cxv_f411, cxv_f141, cxv_f114, cxv_f4101, cxv_f4011
!..data
integer, parameter, public :: &
  cxv_nb411(-1:ubound(mgv411_nb,dim=1))=(/0,mgv411_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b411.f90'
include 'cxv_f411.f90'
include 'cxv_f141.f90'
include 'cxv_f114.f90'
include 'cxv_f4101.f90'
include 'cxv_f4011.f90'
END MODULE inv_cxv411
