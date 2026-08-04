MODULE inv_cxv532
!..use and access
use inv_core
use inv_mg532
use inv_mgv532
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b532, cxv_f532
!..data
integer, parameter, public :: &
  cxv_nb532(-1:ubound(mgv532_nb,dim=1))=(/0,mgv532_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b532.f90'
include 'cxv_f532.f90'
END MODULE inv_cxv532
