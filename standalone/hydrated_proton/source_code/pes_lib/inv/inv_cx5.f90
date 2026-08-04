MODULE inv_cx5
!..use and access
use inv_core
use inv_mg5
use inv_cxx
implicit none
private
!..procedures
public :: cx_b5, cx_f5, cx_f05, cx_f005
!..data
integer, parameter, public :: &
  cx_nb5(-1:ubound(mg5_nb,dim=1))=(/0,mg5_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b5.f90'
include 'cx_f5.f90'
include 'cx_f05.f90'
include 'cx_f005.f90'
END MODULE inv_cx5
