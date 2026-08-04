MODULE inv_cx222211
!..use and access
use inv_core
use inv_mg222211
use inv_cxx
implicit none
private
!..procedures
public :: cx_b222211, cx_f222211
!..data
integer, parameter, public :: &
  cx_nb222211(-1:ubound(mg222211_nb,dim=1))=(/0,mg222211_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b222211.f90'
include 'cx_f222211.f90'
END MODULE inv_cx222211
