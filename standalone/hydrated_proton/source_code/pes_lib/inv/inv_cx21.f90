MODULE inv_cx21
!..use and access
use inv_core
use inv_mg21
use inv_cxx
implicit none
private
!..procedures
public :: cx_b21, cx_f21, cx_f12, cx_f201, cx_f021, cx_f102, cx_f012, &
  cx_f2001, cx_f0201
!..data
integer, parameter, public :: &
  cx_nb21(-1:ubound(mg21_nb,dim=1))=(/0,mg21_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b21.f90'
include 'cx_f21.f90'
include 'cx_f12.f90'
include 'cx_f201.f90'
include 'cx_f021.f90'
include 'cx_f102.f90'
include 'cx_f012.f90'
include 'cx_f2001.f90'
include 'cx_f0201.f90'
END MODULE inv_cx21
