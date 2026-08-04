MODULE inv_cx31111
!..use and access
use inv_core
use inv_mg31111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b31111, cx_f31111
!..data
integer, parameter, public :: &
  cx_nb31111(-1:ubound(mg31111_nb,dim=1))=(/0,mg31111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b31111.f90'
include 'cx_f31111.f90'
END MODULE inv_cx31111
