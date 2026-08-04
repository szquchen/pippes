MODULE inv_cx22
!..use and access
use inv_core
use inv_mg22
use inv_cxx
implicit none
private
!..procedures
public :: cx_b22, cx_f22, cx_f202, cx_f022
!..data
integer, parameter, public :: &
  cx_nb22(-1:ubound(mg22_nb,dim=1))=(/0,mg22_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b22.f90'
include 'cx_f22.f90'
include 'cx_f202.f90'
include 'cx_f022.f90'
END MODULE inv_cx22
