MODULE inv_cx72
!..use and access
use inv_core
use inv_mg72
use inv_cxx
implicit none
private
!..procedures
public :: cx_b72, cx_f72
!..data
integer, parameter, public :: &
  cx_nb72(-1:ubound(mg72_nb,dim=1))=(/0,mg72_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b72.f90'
include 'cx_f72.f90'
END MODULE inv_cx72
