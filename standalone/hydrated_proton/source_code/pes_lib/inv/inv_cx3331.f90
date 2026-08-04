MODULE inv_cx3331
!..use and access
use inv_core
use inv_mg3331
use inv_cxx
implicit none
private
!..procedures
public :: cx_b3331, cx_f3331
!..data
integer, parameter, public :: &
  cx_nb3331(-1:ubound(mg3331_nb,dim=1))=(/0,mg3331_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b3331.f90'
include 'cx_f3331.f90'
END MODULE inv_cx3331
