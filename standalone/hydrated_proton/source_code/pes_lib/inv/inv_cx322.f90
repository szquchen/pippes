MODULE inv_cx322
!..use and access
use inv_core
use inv_mg322
use inv_cxx
implicit none
private
!..procedures
public :: cx_b322, cx_f322, cx_f232, cx_f223
!..data
integer, parameter, public :: &
  cx_nb322(-1:ubound(mg322_nb,dim=1))=(/0,mg322_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b322.f90'
include 'cx_f322.f90'
include 'cx_f232.f90'
include 'cx_f223.f90'
END MODULE inv_cx322
