MODULE inv_cx1
!..use and access
use inv_core
use inv_mg1
use inv_cxx
implicit none
private
!..procedures
public :: cx_b1, cx_f1, cx_f01, cx_f001, cx_f0001
!..data
integer, parameter, public :: &
  cx_nb1=1
!..procedures
CONTAINS
include 'cx_b1.f90'
include 'cx_f1.f90'
include 'cx_f01.f90'
include 'cx_f001.f90'
include 'cx_f0001.f90'
END MODULE inv_cx1
