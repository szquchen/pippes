MODULE inv_cx711
!..use and access
use inv_core
use inv_mg711
use inv_cxx
implicit none
private
!..procedures
public :: cx_b711, cx_f711
!..data
integer, parameter, public :: &
  cx_nb711(-1:ubound(mg711_nb,dim=1))=(/0,mg711_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b711.f90'
include 'cx_f711.f90'
END MODULE inv_cx711
