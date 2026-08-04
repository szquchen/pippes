MODULE inv_cx4
!..use and access
use inv_core
use inv_mg4
use inv_cxx
implicit none
private
!..procedures
public :: cx_b4, cx_f4, cx_f04, cx_f004
!..data
integer, parameter, public :: &
  cx_nb4(-1:ubound(mg4_nb,dim=1))=(/0,mg4_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b4.f90'
include 'cx_f4.f90'
include 'cx_f04.f90'
include 'cx_f004.f90'
END MODULE inv_cx4
