MODULE inv_cx3222
!..use and access
use inv_core
use inv_mg3222
use inv_cxx
implicit none
private
!..procedures
public :: cx_b3222, cx_f3222
!..data
integer, parameter, public :: &
  cx_nb3222(-1:ubound(mg3222_nb,dim=1))=(/0,mg3222_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b3222.f90'
include 'cx_f3222.f90'
END MODULE inv_cx3222
