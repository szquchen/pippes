MODULE inv_cxv41
!..use and access
use inv_core
use inv_mg41
use inv_mgv41
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b41, cxv_f41, cxv_f14, cxv_f401, cxv_f041, cxv_f104, cxv_f014, &
  cxv_f4001
!..data
integer, parameter, public :: &
  cxv_nb41(-1:ubound(mgv41_nb,dim=1))=(/0,mgv41_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b41.f90'
include 'cxv_f41.f90'
include 'cxv_f14.f90'
include 'cxv_f401.f90'
include 'cxv_f041.f90'
include 'cxv_f104.f90'
include 'cxv_f014.f90'
include 'cxv_f4001.f90'
END MODULE inv_cxv41
