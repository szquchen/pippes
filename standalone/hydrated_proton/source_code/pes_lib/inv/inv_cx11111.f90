MODULE inv_cx11111
!..use and access
use inv_core
use inv_mg11111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b11111, cx_f11111
!..data
integer, parameter, public :: &
  cx_nb11111(-1:ubound(mg11111_nb,dim=1))=(/0,mg11111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b11111.f90'
include 'cx_f11111.f90'
END MODULE inv_cx11111
