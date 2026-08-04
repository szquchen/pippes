MODULE inv_cx31
!..use and access
use inv_core
use inv_mg31
use inv_cxx
implicit none
private
!..procedures
public :: cx_b31, cx_f31, cx_f13, cx_f301, cx_f031, cx_f103, cx_f013, &
  cx_f3001
!..data
integer, parameter, public :: &
  cx_nb31(-1:ubound(mg31_nb,dim=1))=(/0,mg31_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b31.f90'
include 'cx_f31.f90'
include 'cx_f13.f90'
include 'cx_f301.f90'
include 'cx_f031.f90'
include 'cx_f103.f90'
include 'cx_f013.f90'
include 'cx_f3001.f90'
END MODULE inv_cx31
