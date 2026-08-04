MODULE inv_cx531
!..use and access
use inv_core
use inv_mg531
use inv_cxx
implicit none
private
!..procedures
public :: cx_b531, cx_f531
!..data
integer, parameter, public :: &
  cx_nb531(-1:ubound(mg531_nb,dim=1))=(/0,mg531_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b531.f90'
include 'cx_f531.f90'
END MODULE inv_cx531
