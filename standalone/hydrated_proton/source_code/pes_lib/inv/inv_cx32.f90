MODULE inv_cx32
!..use and access
use inv_core
use inv_mg32
use inv_cxx
implicit none
private
!..procedures
public :: cx_b32, cx_f32, cx_f23, cx_f302, cx_f032, cx_f203, cx_f023
!..data
integer, parameter, public :: &
  cx_nb32(-1:ubound(mg32_nb,dim=1))=(/0,mg32_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b32.f90'
include 'cx_f32.f90'
include 'cx_f23.f90'
include 'cx_f302.f90'
include 'cx_f032.f90'
include 'cx_f203.f90'
include 'cx_f023.f90'
END MODULE inv_cx32
