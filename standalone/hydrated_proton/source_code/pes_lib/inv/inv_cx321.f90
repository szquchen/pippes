MODULE inv_cx321
!..use and access
use inv_core
use inv_mg321
use inv_cxx
implicit none
private
!..procedures
public :: cx_b321, cx_f321, cx_f231, cx_f312, cx_f132, cx_f213, cx_f123, &
  cx_f3201
!..data
integer, parameter, public :: &
  cx_nb321(-1:ubound(mg321_nb,dim=1))=(/0,mg321_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b321.f90'
include 'cx_f321.f90'
include 'cx_f231.f90'
include 'cx_f312.f90'
include 'cx_f132.f90'
include 'cx_f213.f90'
include 'cx_f123.f90'
include 'cx_f3201.f90'
END MODULE inv_cx321
