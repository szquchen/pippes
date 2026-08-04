MODULE inv_cx421
!..use and access
use inv_core
use inv_mg421
use inv_cxx
implicit none
private
!..procedures
public :: cx_b421, cx_f421, cx_f412, cx_f4201
!..data
integer, parameter, public :: &
  cx_nb421(-1:ubound(mg421_nb,dim=1))=(/0,mg421_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b421.f90'
include 'cx_f421.f90'
include 'cx_f412.f90'
include 'cx_f4201.f90'
END MODULE inv_cx421
