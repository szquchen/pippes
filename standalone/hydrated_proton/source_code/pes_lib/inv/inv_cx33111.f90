MODULE inv_cx33111
!..use and access
use inv_core
use inv_mg33111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b33111, cx_f33111
!..data
integer, parameter, public :: &
  cx_nb33111(-1:ubound(mg33111_nb,dim=1))=(/0,mg33111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b33111.f90'
include 'cx_f33111.f90'
END MODULE inv_cx33111
