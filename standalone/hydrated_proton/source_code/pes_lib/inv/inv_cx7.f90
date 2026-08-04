MODULE inv_cx7
!..use and access
use inv_core
use inv_mg7
use inv_cxx
implicit none
private
!..procedures
public :: cx_b7, cx_f7
!..data
integer, parameter, public :: &
  cx_nb7(-1:ubound(mg7_nb,dim=1))=(/0,mg7_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b7.f90'
include 'cx_f7.f90'
END MODULE inv_cx7
