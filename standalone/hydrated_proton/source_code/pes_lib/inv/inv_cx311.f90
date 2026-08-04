MODULE inv_cx311
!..use and access
use inv_core
use inv_mg311
use inv_cxx
implicit none
private
!..procedures
public :: cx_b311, cx_f311, cx_f131, cx_f113, cx_f3101, cx_f3011
!..data
integer, parameter, public :: &
  cx_nb311(-1:ubound(mg311_nb,dim=1))=(/0,mg311_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b311.f90'
include 'cx_f311.f90'
include 'cx_f131.f90'
include 'cx_f113.f90'
include 'cx_f3101.f90'
include 'cx_f3011.f90'
END MODULE inv_cx311
