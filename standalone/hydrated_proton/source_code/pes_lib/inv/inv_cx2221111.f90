MODULE inv_cx2221111
!..use and access
use inv_core
use inv_mg2221111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b2221111, cx_f2221111
!..data
integer, parameter, public :: &
  cx_nb2221111(-1:ubound(mg2221111_nb,dim=1))=(/0,mg2221111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b2221111.f90'
include 'cx_f2221111.f90'
END MODULE inv_cx2221111
