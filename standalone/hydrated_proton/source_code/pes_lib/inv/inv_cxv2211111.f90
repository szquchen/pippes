MODULE inv_cxv2211111
!..use and access
use inv_core
use inv_mg2211111
use inv_mgv2211111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b2211111, cxv_f2211111
!..data
integer, parameter, public :: &
  cxv_nb2211111(-1:ubound(mgv2211111_nb,dim=1))=(/0,mgv2211111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b2211111.f90'
include 'cxv_f2211111.f90'
END MODULE inv_cxv2211111
