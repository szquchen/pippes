MODULE inv_cx11
!..use and access
use inv_core
use inv_mg11
use inv_cxx
implicit none
private
!..procedures
public :: cx_b11, cx_f11, cx_f101, cx_f011, cx_f1001, cx_f0101, cx_f0011
!..data
integer, parameter, public :: &
  cx_nb11(-1:ubound(mg11_nb,dim=1))=(/0,mg11_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b11.f90'
include 'cx_f11.f90'
include 'cx_f101.f90'
include 'cx_f011.f90'
include 'cx_f1001.f90'
include 'cx_f0101.f90'
include 'cx_f0011.f90'
END MODULE inv_cx11
