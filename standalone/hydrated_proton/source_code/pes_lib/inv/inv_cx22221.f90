MODULE inv_cx22221
!..use and access
use inv_core
use inv_mg22221
use inv_cxx
implicit none
private
!..procedures
public :: cx_b22221, cx_f22221
!..data
integer, parameter, public :: &
  cx_nb22221(-1:ubound(mg22221_nb,dim=1))=(/0,mg22221_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b22221.f90'
include 'cx_f22221.f90'
END MODULE inv_cx22221
