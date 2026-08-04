MODULE inv_cxv62
!..use and access
use inv_core
use inv_mg62
use inv_mgv62
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b62, cxv_f62
!..data
integer, parameter, public :: &
  cxv_nb62(-1:ubound(mgv62_nb,dim=1))=(/0,mgv62_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b62.f90'
include 'cxv_f62.f90'
END MODULE inv_cxv62
