MODULE inv_cx2111
!..use and access
use inv_core
use inv_mg2111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b2111, cx_f2111, cx_f1211
!..data
integer, parameter, public :: &
  cx_nb2111(-1:ubound(mg2111_nb,dim=1))=(/0,mg2111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b2111.f90'
include 'cx_f2111.f90'
include 'cx_f1211.f90'
END MODULE inv_cx2111
