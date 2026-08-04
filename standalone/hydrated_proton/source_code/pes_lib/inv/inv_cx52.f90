MODULE inv_cx52
!..use and access
use inv_core
use inv_mg52
use inv_cxx
implicit none
private
!..procedures
public :: cx_b52, cx_f52
!..data
integer, parameter, public :: &
  cx_nb52(-1:ubound(mg52_nb,dim=1))=(/0,mg52_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b52.f90'
include 'cx_f52.f90'
END MODULE inv_cx52
