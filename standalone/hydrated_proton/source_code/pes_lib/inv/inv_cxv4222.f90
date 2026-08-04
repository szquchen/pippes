MODULE inv_cxv4222
!..use and access
use inv_core
use inv_mg4222
use inv_mgv4222
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b4222, cxv_f4222
!..data
integer, parameter, public :: &
  cxv_nb4222(-1:ubound(mgv4222_nb,dim=1))=(/0,mgv4222_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b4222.f90'
include 'cxv_f4222.f90'
END MODULE inv_cxv4222
