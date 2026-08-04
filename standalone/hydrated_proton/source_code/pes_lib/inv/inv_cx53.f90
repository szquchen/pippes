MODULE inv_cx53
!..use and access
use inv_core
use inv_mg53
use inv_cxx
implicit none
private
!..procedures
public :: cx_b53, cx_f53
!..data
integer, parameter, public :: &
  cx_nb53(-1:ubound(mg53_nb,dim=1))=(/0,mg53_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b53.f90'
include 'cx_f53.f90'
END MODULE inv_cx53
