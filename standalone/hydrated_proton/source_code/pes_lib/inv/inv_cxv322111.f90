MODULE inv_cxv322111
!..use and access
use inv_core
use inv_mg322111
use inv_mgv322111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b322111, cxv_f322111
!..data
integer, parameter, public :: &
  cxv_nb322111(-1:ubound(mgv322111_nb,dim=1))=(/0,mgv322111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b322111.f90'
include 'cxv_f322111.f90'
END MODULE inv_cxv322111
