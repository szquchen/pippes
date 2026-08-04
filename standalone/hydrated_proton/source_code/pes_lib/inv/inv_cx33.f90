MODULE inv_cx33
!..use and access
use inv_core
use inv_mg33
use inv_cxx
implicit none
private
!..procedures
public :: cx_b33, cx_f33, cx_f303, cx_f033
!..data
integer, parameter, public :: &
  cx_nb33(-1:ubound(mg33_nb,dim=1))=(/0,mg33_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b33.f90'
include 'cx_f33.f90'
include 'cx_f303.f90'
include 'cx_f033.f90'
END MODULE inv_cx33
