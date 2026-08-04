MODULE inv_cx222111
!..use and access
use inv_core
use inv_mg222111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b222111, cx_f222111
!..data
integer, parameter, public :: &
  cx_nb222111(-1:ubound(mg222111_nb,dim=1))=(/0,mg222111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b222111.f90'
include 'cx_f222111.f90'
END MODULE inv_cx222111
