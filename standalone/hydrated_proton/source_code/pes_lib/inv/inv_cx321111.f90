MODULE inv_cx321111
!..use and access
use inv_core
use inv_mg321111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b321111, cx_f321111
!..data
integer, parameter, public :: &
  cx_nb321111(-1:ubound(mg321111_nb,dim=1))=(/0,mg321111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b321111.f90'
include 'cx_f321111.f90'
END MODULE inv_cx321111
