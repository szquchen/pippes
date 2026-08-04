MODULE inv_cx6111
!..use and access
use inv_core
use inv_mg6111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b6111, cx_f6111
!..data
integer, parameter, public :: &
  cx_nb6111(-1:ubound(mg6111_nb,dim=1))=(/0,mg6111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b6111.f90'
include 'cx_f6111.f90'
END MODULE inv_cx6111
