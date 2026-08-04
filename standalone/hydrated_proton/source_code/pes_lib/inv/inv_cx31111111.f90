MODULE inv_cx31111111
!..use and access
use inv_core
use inv_mg31111111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b31111111, cx_f31111111
!..data
integer, parameter, public :: &
  cx_nb31111111(-1:ubound(mg31111111_nb,dim=1))=(/0,mg31111111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b31111111.f90'
include 'cx_f31111111.f90'
END MODULE inv_cx31111111
