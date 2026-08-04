MODULE inv_cx43
!..use and access
use inv_core
use inv_mg43
use inv_cxx
implicit none
private
!..procedures
public :: cx_b43, cx_f43
!..data
integer, parameter, public :: &
  cx_nb43(-1:ubound(mg43_nb,dim=1))=(/0,mg43_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b43.f90'
include 'cx_f43.f90'
END MODULE inv_cx43
