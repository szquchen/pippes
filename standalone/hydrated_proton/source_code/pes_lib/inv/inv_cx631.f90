MODULE inv_cx631
!..use and access
use inv_core
use inv_mg631
use inv_cxx
implicit none
private
!..procedures
public :: cx_b631, cx_f631
!..data
integer, parameter, public :: &
  cx_nb631(-1:ubound(mg631_nb,dim=1))=(/0,mg631_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b631.f90'
include 'cx_f631.f90'
END MODULE inv_cx631
