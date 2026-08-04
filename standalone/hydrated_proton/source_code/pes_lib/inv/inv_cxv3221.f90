MODULE inv_cxv3221
!..use and access
use inv_core
use inv_mg3221
use inv_mgv3221
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b3221, cxv_f3221
!..data
integer, parameter, public :: &
  cxv_nb3221(-1:ubound(mgv3221_nb,dim=1))=(/0,mgv3221_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b3221.f90'
include 'cxv_f3221.f90'
END MODULE inv_cxv3221
