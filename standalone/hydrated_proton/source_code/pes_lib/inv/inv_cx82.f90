MODULE inv_cx82
!..use and access
use inv_core
use inv_mg82
use inv_cxx
implicit none
private
!..procedures
public :: cx_b82, cx_f82
!..data
integer, parameter, public :: &
  cx_nb82(-1:ubound(mg82_nb,dim=1))=(/0,mg82_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b82.f90'
include 'cx_f82.f90'
END MODULE inv_cx82
