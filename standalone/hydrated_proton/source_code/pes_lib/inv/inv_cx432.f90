MODULE inv_cx432
!..use and access
use inv_core
use inv_mg432
use inv_cxx
implicit none
private
!..procedures
public :: cx_b432, cx_f432
!..data
integer, parameter, public :: &
  cx_nb432(-1:ubound(mg432_nb,dim=1))=(/0,mg432_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b432.f90'
include 'cx_f432.f90'
END MODULE inv_cx432
