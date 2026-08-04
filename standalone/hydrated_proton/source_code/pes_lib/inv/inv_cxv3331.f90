MODULE inv_cxv3331
!..use and access
use inv_core
use inv_mg3331
use inv_mgv3331
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b3331, cxv_f3331
!..data
integer, parameter, public :: &
  cxv_nb3331(-1:ubound(mgv3331_nb,dim=1))=(/0,mgv3331_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b3331.f90'
include 'cxv_f3331.f90'
END MODULE inv_cxv3331
