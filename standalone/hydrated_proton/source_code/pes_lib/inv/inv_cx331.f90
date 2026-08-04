MODULE inv_cx331
!..use and access
use inv_core
use inv_mg331
use inv_cxx
implicit none
private
!..procedures
public :: cx_b331, cx_f331, cx_f313, cx_f133
!..data
integer, parameter, public :: &
  cx_nb331(-1:ubound(mg331_nb,dim=1))=(/0,mg331_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b331.f90'
include 'cx_f331.f90'
include 'cx_f313.f90'
include 'cx_f133.f90'
END MODULE inv_cx331
