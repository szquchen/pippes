MODULE inv_cx4321
!..use and access
use inv_core
use inv_mg4321
use inv_cxx
implicit none
private
!..procedures
public :: cx_b4321, cx_f4321
!..data
integer, parameter, public :: &
  cx_nb4321(-1:ubound(mg4321_nb,dim=1))=(/0,mg4321_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b4321.f90'
include 'cx_f4321.f90'
END MODULE inv_cx4321
