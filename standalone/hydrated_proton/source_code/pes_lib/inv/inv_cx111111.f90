MODULE inv_cx111111
!..use and access
use inv_core
use inv_mg111111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b111111, cx_f111111
!..data
integer, parameter, public :: &
  cx_nb111111(-1:ubound(mg111111_nb,dim=1))=(/0,mg111111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b111111.f90'
include 'cx_f111111.f90'
END MODULE inv_cx111111
