MODULE inv_cx73
!..use and access
use inv_core
use inv_mg73
use inv_cxx
implicit none
private
!..procedures
public :: cx_b73, cx_f73
!..data
integer, parameter, public :: &
  cx_nb73(-1:ubound(mg73_nb,dim=1))=(/0,mg73_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b73.f90'
include 'cx_f73.f90'
END MODULE inv_cx73
