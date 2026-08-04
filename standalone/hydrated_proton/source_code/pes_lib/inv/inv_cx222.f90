MODULE inv_cx222
!..use and access
use inv_core
use inv_mg222
use inv_cxx
implicit none
private
!..procedures
public :: cx_b222, cx_f222
!..data
integer, parameter, public :: &
  cx_nb222(-1:ubound(mg222_nb,dim=1))=(/0,mg222_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b222.f90'
include 'cx_f222.f90'
END MODULE inv_cx222
