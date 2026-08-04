MODULE inv_cx5111
!..use and access
use inv_core
use inv_mg5111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b5111, cx_f5111
!..data
integer, parameter, public :: &
  cx_nb5111(-1:ubound(mg5111_nb,dim=1))=(/0,mg5111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b5111.f90'
include 'cx_f5111.f90'
END MODULE inv_cx5111
