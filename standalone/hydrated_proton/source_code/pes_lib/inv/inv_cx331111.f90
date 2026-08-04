MODULE inv_cx331111
!..use and access
use inv_core
use inv_mg331111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b331111, cx_f331111
!..data
integer, parameter, public :: &
  cx_nb331111(-1:ubound(mg331111_nb,dim=1))=(/0,mg331111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b331111.f90'
include 'cx_f331111.f90'
END MODULE inv_cx331111
