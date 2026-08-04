MODULE inv_cx421111
!..use and access
use inv_core
use inv_mg421111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b421111, cx_f421111
!..data
integer, parameter, public :: &
  cx_nb421111(-1:ubound(mg421111_nb,dim=1))=(/0,mg421111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b421111.f90'
include 'cx_f421111.f90'
END MODULE inv_cx421111
