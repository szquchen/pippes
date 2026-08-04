MODULE inv_cxv222111
!..use and access
use inv_core
use inv_mg222111
use inv_mgv222111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b222111, cxv_f222111
!..data
integer, parameter, public :: &
  cxv_nb222111(-1:ubound(mgv222111_nb,dim=1))=(/0,mgv222111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b222111.f90'
include 'cxv_f222111.f90'
END MODULE inv_cxv222111
