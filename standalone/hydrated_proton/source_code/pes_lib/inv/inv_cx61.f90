MODULE inv_cx61
!..use and access
use inv_core
use inv_mg61
use inv_cxx
implicit none
private
!..procedures
public :: cx_b61, cx_f61, cx_f601, cx_f6001
!..data
integer, parameter, public :: &
  cx_nb61(-1:ubound(mg61_nb,dim=1))=(/0,mg61_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b61.f90'
include 'cx_f61.f90'
include 'cx_f601.f90'
include 'cx_f6001.f90'
END MODULE inv_cx61
