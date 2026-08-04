MODULE inv_cx1111111
!..use and access
use inv_core
use inv_mg1111111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b1111111, cx_f1111111
!..data
integer, parameter, public :: &
  cx_nb1111111(-1:ubound(mg1111111_nb,dim=1))=(/0,mg1111111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b1111111.f90'
include 'cx_f1111111.f90'
END MODULE inv_cx1111111
