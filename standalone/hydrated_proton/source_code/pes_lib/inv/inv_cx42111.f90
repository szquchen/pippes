MODULE inv_cx42111
!..use and access
use inv_core
use inv_mg42111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b42111, cx_f42111
!..data
integer, parameter, public :: &
  cx_nb42111(-1:ubound(mg42111_nb,dim=1))=(/0,mg42111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b42111.f90'
include 'cx_f42111.f90'
END MODULE inv_cx42111
