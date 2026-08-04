MODULE inv_cx521
!..use and access
use inv_core
use inv_mg521
use inv_cxx
implicit none
private
!..procedures
public :: cx_b521, cx_f521
!..data
integer, parameter, public :: &
  cx_nb521(-1:ubound(mg521_nb,dim=1))=(/0,mg521_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b521.f90'
include 'cx_f521.f90'
END MODULE inv_cx521
