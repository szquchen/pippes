MODULE inv_cx611
!..use and access
use inv_core
use inv_mg611
use inv_cxx
implicit none
private
!..procedures
public :: cx_b611, cx_f611, cx_f6101, cx_f6011
!..data
integer, parameter, public :: &
  cx_nb611(-1:ubound(mg611_nb,dim=1))=(/0,mg611_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b611.f90'
include 'cx_f611.f90'
include 'cx_f6101.f90'
include 'cx_f6011.f90'
END MODULE inv_cx611
