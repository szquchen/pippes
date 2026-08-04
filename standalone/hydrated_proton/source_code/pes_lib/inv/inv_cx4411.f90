MODULE inv_cx4411
!..use and access
use inv_core
use inv_mg4411
use inv_cxx
implicit none
private
!..procedures
public :: cx_b4411, cx_f4411
!..data
integer, parameter, public :: &
  cx_nb4411(-1:ubound(mg4411_nb,dim=1))=(/0,mg4411_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b4411.f90'
include 'cx_f4411.f90'
END MODULE inv_cx4411
