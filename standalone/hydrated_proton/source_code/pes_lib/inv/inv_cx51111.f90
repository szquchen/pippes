MODULE inv_cx51111
!..use and access
use inv_core
use inv_mg51111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b51111, cx_f51111
!..data
integer, parameter, public :: &
  cx_nb51111(-1:ubound(mg51111_nb,dim=1))=(/0,mg51111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b51111.f90'
include 'cx_f51111.f90'
END MODULE inv_cx51111
