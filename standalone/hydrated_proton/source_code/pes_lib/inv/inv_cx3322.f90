MODULE inv_cx3322
!..use and access
use inv_core
use inv_mg3322
use inv_cxx
implicit none
private
!..procedures
public :: cx_b3322, cx_f3322
!..data
integer, parameter, public :: &
  cx_nb3322(-1:ubound(mg3322_nb,dim=1))=(/0,mg3322_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b3322.f90'
include 'cx_f3322.f90'
END MODULE inv_cx3322
