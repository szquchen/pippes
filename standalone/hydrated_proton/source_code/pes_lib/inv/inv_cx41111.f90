MODULE inv_cx41111
!..use and access
use inv_core
use inv_mg41111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b41111, cx_f41111
!..data
integer, parameter, public :: &
  cx_nb41111(-1:ubound(mg41111_nb,dim=1))=(/0,mg41111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b41111.f90'
include 'cx_f41111.f90'
END MODULE inv_cx41111
