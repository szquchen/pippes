MODULE inv_cx1111
!..use and access
use inv_core
use inv_mg1111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b1111, cx_f1111
!..data
integer, parameter, public :: &
  cx_nb1111(-1:ubound(mg1111_nb,dim=1))=(/0,mg1111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b1111.f90'
include 'cx_f1111.f90'
END MODULE inv_cx1111
