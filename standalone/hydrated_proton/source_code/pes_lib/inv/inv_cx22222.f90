MODULE inv_cx22222
!..use and access
use inv_core
use inv_mg22222
use inv_cxx
implicit none
private
!..procedures
public :: cx_b22222, cx_f22222
!..data
integer, parameter, public :: &
  cx_nb22222(-1:ubound(mg22222_nb,dim=1))=(/0,mg22222_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b22222.f90'
include 'cx_f22222.f90'
END MODULE inv_cx22222
