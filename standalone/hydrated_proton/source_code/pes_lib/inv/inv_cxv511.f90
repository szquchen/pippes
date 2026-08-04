MODULE inv_cxv511
!..use and access
use inv_core
use inv_mg511
use inv_mgv511
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b511, cxv_f511, cxv_f5101, cxv_f5011
!..data
integer, parameter, public :: &
  cxv_nb511(-1:ubound(mgv511_nb,dim=1))=(/0,mgv511_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b511.f90'
include 'cxv_f511.f90'
include 'cxv_f5101.f90'
include 'cxv_f5011.f90'
END MODULE inv_cxv511
