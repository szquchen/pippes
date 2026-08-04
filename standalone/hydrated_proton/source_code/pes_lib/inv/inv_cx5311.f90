MODULE inv_cx5311
!..use and access
use inv_core
use inv_mg5311
use inv_cxx
implicit none
private
!..procedures
public :: cx_b5311, cx_f5311
!..data
integer, parameter, public :: &
  cx_nb5311(-1:ubound(mg5311_nb,dim=1))=(/0,mg5311_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b5311.f90'
include 'cx_f5311.f90'
END MODULE inv_cx5311
