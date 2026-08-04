MODULE inv_cxv3222
!..use and access
use inv_core
use inv_mg3222
use inv_mgv3222
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b3222, cxv_f3222
!..data
integer, parameter, public :: &
  cxv_nb3222(-1:ubound(mgv3222_nb,dim=1))=(/0,mgv3222_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b3222.f90'
include 'cxv_f3222.f90'
END MODULE inv_cxv3222
