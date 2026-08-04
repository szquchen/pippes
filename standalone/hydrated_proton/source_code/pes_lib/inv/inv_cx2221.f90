MODULE inv_cx2221
!..use and access
use inv_core
use inv_mg2221
use inv_cxx
implicit none
private
!..procedures
public :: cx_b2221, cx_f2221
!..data
integer, parameter, public :: &
  cx_nb2221(-1:ubound(mg2221_nb,dim=1))=(/0,mg2221_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b2221.f90'
include 'cx_f2221.f90'
END MODULE inv_cx2221
