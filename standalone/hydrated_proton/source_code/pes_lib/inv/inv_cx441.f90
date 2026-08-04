MODULE inv_cx441
!..use and access
use inv_core
use inv_mg441
use inv_cxx
implicit none
private
!..procedures
public :: cx_b441, cx_f441
!..data
integer, parameter, public :: &
  cx_nb441(-1:ubound(mg441_nb,dim=1))=(/0,mg441_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b441.f90'
include 'cx_f441.f90'
END MODULE inv_cx441
