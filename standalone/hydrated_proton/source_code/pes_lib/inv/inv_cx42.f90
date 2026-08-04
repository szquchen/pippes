MODULE inv_cx42
!..use and access
use inv_core
use inv_mg42
use inv_cxx
implicit none
private
!..procedures
public :: cx_b42, cx_f42, cx_f24, cx_f402, cx_f042, cx_f204, cx_f024
!..data
integer, parameter, public :: &
  cx_nb42(-1:ubound(mg42_nb,dim=1))=(/0,mg42_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b42.f90'
include 'cx_f42.f90'
include 'cx_f24.f90'
include 'cx_f402.f90'
include 'cx_f042.f90'
include 'cx_f204.f90'
include 'cx_f024.f90'
END MODULE inv_cx42
