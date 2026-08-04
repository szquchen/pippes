MODULE inv_cx221111
!..use and access
use inv_core
use inv_mg221111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b221111, cx_f221111
!..data
integer, parameter, public :: &
  cx_nb221111(-1:ubound(mg221111_nb,dim=1))=(/0,mg221111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b221111.f90'
include 'cx_f221111.f90'
END MODULE inv_cx221111
