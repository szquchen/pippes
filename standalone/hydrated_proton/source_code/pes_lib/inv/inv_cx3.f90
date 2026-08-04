MODULE inv_cx3
!..use and access
use inv_core
use inv_mg3
use inv_cxx
implicit none
private
!..procedures
public :: cx_b3, cx_f3, cx_f03, cx_f003
!..data
integer, parameter, public :: &
  cx_nb3(-1:ubound(mg3_nb,dim=1))=(/0,mg3_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b3.f90'
include 'cx_f3.f90'
include 'cx_f03.f90'
include 'cx_f003.f90'
END MODULE inv_cx3
