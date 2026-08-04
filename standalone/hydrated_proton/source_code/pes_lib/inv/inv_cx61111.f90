MODULE inv_cx61111
!..use and access
use inv_core
use inv_mg61111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b61111, cx_f61111
!..data
integer, parameter, public :: &
  cx_nb61111(-1:ubound(mg61111_nb,dim=1))=(/0,mg61111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b61111.f90'
include 'cx_f61111.f90'
END MODULE inv_cx61111
