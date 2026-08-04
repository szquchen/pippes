MODULE inv_cxv721
!..use and access
use inv_core
use inv_mg721
use inv_mgv721
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b721, cxv_f721
!..data
integer, parameter, public :: &
  cxv_nb721(-1:ubound(mgv721_nb,dim=1))=(/0,mgv721_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b721.f90'
include 'cxv_f721.f90'
END MODULE inv_cxv721
