MODULE inv_cx21111
!..use and access
use inv_core
use inv_mg21111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b21111, cx_f21111
!..data
integer, parameter, public :: &
  cx_nb21111(-1:ubound(mg21111_nb,dim=1))=(/0,mg21111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b21111.f90'
include 'cx_f21111.f90'
END MODULE inv_cx21111
