MODULE inv_cxv53
!..use and access
use inv_core
use inv_mg53
use inv_mgv53
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b53, cxv_f53
!..data
integer, parameter, public :: &
  cxv_nb53(-1:ubound(mgv53_nb,dim=1))=(/0,mgv53_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b53.f90'
include 'cxv_f53.f90'
END MODULE inv_cxv53
