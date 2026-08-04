MODULE inv_cx2111111
!..use and access
use inv_core
use inv_mg2111111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b2111111, cx_f2111111
!..data
integer, parameter, public :: &
  cx_nb2111111(-1:ubound(mg2111111_nb,dim=1))=(/0,mg2111111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b2111111.f90'
include 'cx_f2111111.f90'
END MODULE inv_cx2111111
