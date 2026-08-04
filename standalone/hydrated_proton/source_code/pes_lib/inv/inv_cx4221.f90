MODULE inv_cx4221
!..use and access
use inv_core
use inv_mg4221
use inv_cxx
implicit none
private
!..procedures
public :: cx_b4221, cx_f4221
!..data
integer, parameter, public :: &
  cx_nb4221(-1:ubound(mg4221_nb,dim=1))=(/0,mg4221_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b4221.f90'
include 'cx_f4221.f90'
END MODULE inv_cx4221
