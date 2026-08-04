MODULE inv_cxv3
!..use and access
use inv_core
use inv_mg3
use inv_mgv3
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b3, cxv_f3, cxv_f03, cxv_f003
!..data
integer, parameter, public :: &
  cxv_nb3(-1:ubound(mgv3_nb,dim=1))=(/0,mgv3_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b3.f90'
include 'cxv_f3.f90'
include 'cxv_f03.f90'
include 'cxv_f003.f90'
END MODULE inv_cxv3
