MODULE inv_cx332
!..use and access
use inv_core
use inv_mg332
use inv_cxx
implicit none
private
!..procedures
public :: cx_b332, cx_f332, cx_f323, cx_f233
!..data
integer, parameter, public :: &
  cx_nb332(-1:ubound(mg332_nb,dim=1))=(/0,mg332_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b332.f90'
include 'cx_f332.f90'
include 'cx_f323.f90'
include 'cx_f233.f90'
END MODULE inv_cx332
