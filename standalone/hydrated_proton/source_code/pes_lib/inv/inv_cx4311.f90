MODULE inv_cx4311
!..use and access
use inv_core
use inv_mg4311
use inv_cxx
implicit none
private
!..procedures
public :: cx_b4311, cx_f4311
!..data
integer, parameter, public :: &
  cx_nb4311(-1:ubound(mg4311_nb,dim=1))=(/0,mg4311_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b4311.f90'
include 'cx_f4311.f90'
END MODULE inv_cx4311
