MODULE inv_cx32111
!..use and access
use inv_core
use inv_mg32111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b32111, cx_f32111
!..data
integer, parameter, public :: &
  cx_nb32111(-1:ubound(mg32111_nb,dim=1))=(/0,mg32111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b32111.f90'
include 'cx_f32111.f90'
END MODULE inv_cx32111
