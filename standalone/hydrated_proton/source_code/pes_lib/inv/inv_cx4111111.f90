MODULE inv_cx4111111
!..use and access
use inv_core
use inv_mg4111111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b4111111, cx_f4111111
!..data
integer, parameter, public :: &
  cx_nb4111111(-1:ubound(mg4111111_nb,dim=1))=(/0,mg4111111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b4111111.f90'
include 'cx_f4111111.f90'
END MODULE inv_cx4111111
