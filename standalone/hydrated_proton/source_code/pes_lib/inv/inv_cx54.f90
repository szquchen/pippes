MODULE inv_cx54
!..use and access
use inv_core
use inv_mg54
use inv_cxx
implicit none
private
!..procedures
public :: cx_b54, cx_f54
!..data
integer, parameter, public :: &
  cx_nb54(-1:ubound(mg54_nb,dim=1))=(/0,mg54_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b54.f90'
include 'cx_f54.f90'
END MODULE inv_cx54
