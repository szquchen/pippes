MODULE inv_cxv332
!..use and access
use inv_core
use inv_mg332
use inv_mgv332
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b332, cxv_f332, cxv_f323, cxv_f233
!..data
integer, parameter, public :: &
  cxv_nb332(-1:ubound(mgv332_nb,dim=1))=(/0,mgv332_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b332.f90'
include 'cxv_f332.f90'
include 'cxv_f323.f90'
include 'cxv_f233.f90'
END MODULE inv_cxv332
