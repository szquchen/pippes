MODULE inv_cx4222
!..use and access
use inv_core
use inv_mg4222
use inv_cxx
implicit none
private
!..procedures
public :: cx_b4222, cx_f4222
!..data
integer, parameter, public :: &
  cx_nb4222(-1:ubound(mg4222_nb,dim=1))=(/0,mg4222_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b4222.f90'
include 'cx_f4222.f90'
END MODULE inv_cx4222
