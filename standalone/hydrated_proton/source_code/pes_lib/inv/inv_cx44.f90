MODULE inv_cx44
!..use and access
use inv_core
use inv_mg44
use inv_cxx
implicit none
private
!..procedures
public :: cx_b44, cx_f44
!..data
integer, parameter, public :: &
  cx_nb44(-1:ubound(mg44_nb,dim=1))=(/0,mg44_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b44.f90'
include 'cx_f44.f90'
END MODULE inv_cx44
