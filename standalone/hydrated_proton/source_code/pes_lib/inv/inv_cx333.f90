MODULE inv_cx333
!..use and access
use inv_core
use inv_mg333
use inv_cxx
implicit none
private
!..procedures
public :: cx_b333, cx_f333
!..data
integer, parameter, public :: &
  cx_nb333(-1:ubound(mg333_nb,dim=1))=(/0,mg333_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b333.f90'
include 'cx_f333.f90'
END MODULE inv_cx333
