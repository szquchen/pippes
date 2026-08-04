MODULE inv_cx2
!..use and access
use inv_core
use inv_mg2
use inv_cxx
implicit none
private
!..procedures
public :: cx_b2, cx_f2, cx_f02, cx_f002
!..data
integer, parameter, public :: &
  cx_nb2(-1:ubound(mg2_nb,dim=1))=(/0,mg2_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b2.f90'
include 'cx_f2.f90'
include 'cx_f02.f90'
include 'cx_f002.f90'
END MODULE inv_cx2
