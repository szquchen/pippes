MODULE inv_cx52111
!..use and access
use inv_core
use inv_mg52111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b52111, cx_f52111
!..data
integer, parameter, public :: &
  cx_nb52111(-1:ubound(mg52111_nb,dim=1))=(/0,mg52111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b52111.f90'
include 'cx_f52111.f90'
END MODULE inv_cx52111
