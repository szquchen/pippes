MODULE inv_cx3221
!..use and access
use inv_core
use inv_mg3221
use inv_cxx
implicit none
private
!..procedures
public :: cx_b3221, cx_f3221
!..data
integer, parameter, public :: &
  cx_nb3221(-1:ubound(mg3221_nb,dim=1))=(/0,mg3221_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b3221.f90'
include 'cx_f3221.f90'
END MODULE inv_cx3221
