MODULE inv_cxv3111
!..use and access
use inv_core
use inv_mg3111
use inv_mgv3111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b3111, cxv_f3111
!..data
integer, parameter, public :: &
  cxv_nb3111(-1:ubound(mgv3111_nb,dim=1))=(/0,mgv3111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b3111.f90'
include 'cxv_f3111.f90'
END MODULE inv_cxv3111
