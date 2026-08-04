MODULE inv_cxv31
!..use and access
use inv_core
use inv_mg31
use inv_mgv31
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b31, cxv_f31, cxv_f13, cxv_f301, cxv_f031, cxv_f103, &
  cxv_f013, cxv_f3001
!..data
integer, parameter, public :: &
  cxv_nb31(-1:ubound(mgv31_nb,dim=1))=(/0,mgv31_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b31.f90'
include 'cxv_f31.f90'
include 'cxv_f13.f90'
include 'cxv_f301.f90'
include 'cxv_f031.f90'
include 'cxv_f103.f90'
include 'cxv_f013.f90'
include 'cxv_f3001.f90'
END MODULE inv_cxv31
