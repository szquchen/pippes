MODULE inv_cx522
!..use and access
use inv_core
use inv_mg522
use inv_cxx
implicit none
private
!..procedures
public :: cx_b522, cx_f522
!..data
integer, parameter, public :: &
  cx_nb522(-1:ubound(mg522_nb,dim=1))=(/0,mg522_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b522.f90'
include 'cx_f522.f90'
END MODULE inv_cx522
