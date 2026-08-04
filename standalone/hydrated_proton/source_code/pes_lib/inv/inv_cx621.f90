MODULE inv_cx621
!..use and access
use inv_core
use inv_mg621
use inv_cxx
implicit none
private
!..procedures
public :: cx_b621, cx_f621
!..data
integer, parameter, public :: &
  cx_nb621(-1:ubound(mg621_nb,dim=1))=(/0,mg621_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b621.f90'
include 'cx_f621.f90'
END MODULE inv_cx621
