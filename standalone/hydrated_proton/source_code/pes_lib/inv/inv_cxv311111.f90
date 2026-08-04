MODULE inv_cxv311111
!..use and access
use inv_core
use inv_mg311111
use inv_mgv311111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b311111, cxv_f311111
!..data
integer, parameter, public :: &
  cxv_nb311111(-1:ubound(mgv311111_nb,dim=1))=(/0,mgv311111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b311111.f90'
include 'cxv_f311111.f90'
END MODULE inv_cxv311111
