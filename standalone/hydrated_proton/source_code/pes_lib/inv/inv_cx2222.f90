MODULE inv_cx2222
!..use and access
use inv_core
use inv_mg2222
use inv_cxx
implicit none
private
!..procedures
public :: cx_b2222, cx_f2222
!..data
integer, parameter, public :: &
  cx_nb2222(-1:ubound(mg2222_nb,dim=1))=(/0,mg2222_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b2222.f90'
include 'cx_f2222.f90'
END MODULE inv_cx2222
