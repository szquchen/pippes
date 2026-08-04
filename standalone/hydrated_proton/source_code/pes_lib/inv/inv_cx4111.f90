MODULE inv_cx4111
!..use and access
use inv_core
use inv_mg4111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b4111, cx_f4111
!..data
integer, parameter, public :: &
  cx_nb4111(-1:ubound(mg4111_nb,dim=1))=(/0,mg4111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b4111.f90'
include 'cx_f4111.f90'
END MODULE inv_cx4111
