MODULE inv_cx63
!..use and access
use inv_core
use inv_mg63
use inv_cxx
implicit none
private
!..procedures
public :: cx_b63, cx_f63
!..data
integer, parameter, public :: &
  cx_nb63(-1:ubound(mg63_nb,dim=1))=(/0,mg63_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b63.f90'
include 'cx_f63.f90'
END MODULE inv_cx63
