MODULE inv_cxv222211
!..use and access
use inv_core
use inv_mg222211
use inv_mgv222211
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b222211, cxv_f222211
!..data
integer, parameter, public :: &
  cxv_nb222211(-1:ubound(mgv222211_nb,dim=1))=(/0,mgv222211_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b222211.f90'
include 'cxv_f222211.f90'
END MODULE inv_cxv222211
