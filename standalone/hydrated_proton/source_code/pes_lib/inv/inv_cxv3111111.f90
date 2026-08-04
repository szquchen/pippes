MODULE inv_cxv3111111
!..use and access
use inv_core
use inv_mg3111111
use inv_mgv3111111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b3111111, cxv_f3111111
!..data
integer, parameter, public :: &
  cxv_nb3111111(-1:ubound(mgv3111111_nb,dim=1))=(/0,mgv3111111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b3111111.f90'
include 'cxv_f3111111.f90'
END MODULE inv_cxv3111111
