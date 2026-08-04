MODULE inv_cxv3211111
!..use and access
use inv_core
use inv_mg3211111
use inv_mgv3211111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b3211111, cxv_f3211111
!..data
integer, parameter, public :: &
  cxv_nb3211111(-1:ubound(mgv3211111_nb,dim=1))=(/0,mgv3211111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b3211111.f90'
include 'cxv_f3211111.f90'
END MODULE inv_cxv3211111
