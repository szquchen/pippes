MODULE inv_cxv222
!..use and access
use inv_core
use inv_mg222
use inv_mgv222
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b222, cxv_f222
!..data
integer, parameter, public :: &
  cxv_nb222(-1:ubound(mgv222_nb,dim=1))=(/0,mgv222_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b222.f90'
include 'cxv_f222.f90'
END MODULE inv_cxv222
