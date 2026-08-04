MODULE inv_cx62
!..use and access
use inv_core
use inv_mg62
use inv_cxx
implicit none
private
!..procedures
public :: cx_b62, cx_f62
!..data
integer, parameter, public :: &
  cx_nb62(-1:ubound(mg62_nb,dim=1))=(/0,mg62_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b62.f90'
include 'cx_f62.f90'
END MODULE inv_cx62
