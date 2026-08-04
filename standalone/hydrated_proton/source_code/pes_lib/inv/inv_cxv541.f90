MODULE inv_cxv541
!..use and access
use inv_core
use inv_mg541
use inv_mgv541
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b541, cxv_f541
!..data
integer, parameter, public :: &
  cxv_nb541(-1:ubound(mgv541_nb,dim=1))=(/0,mgv541_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b541.f90'
include 'cxv_f541.f90'
END MODULE inv_cxv541
