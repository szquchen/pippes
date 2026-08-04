MODULE inv_cx721
!..use and access
use inv_core
use inv_mg721
use inv_cxx
implicit none
private
!..procedures
public :: cx_b721, cx_f721
!..data
integer, parameter, public :: &
  cx_nb721(-1:ubound(mg721_nb,dim=1))=(/0,mg721_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b721.f90'
include 'cx_f721.f90'
END MODULE inv_cx721
