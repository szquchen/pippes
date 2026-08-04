MODULE inv_cx311111
!..use and access
use inv_core
use inv_mg311111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b311111, cx_f311111
!..data
integer, parameter, public :: &
  cx_nb311111(-1:ubound(mg311111_nb,dim=1))=(/0,mg311111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b311111.f90'
include 'cx_f311111.f90'
END MODULE inv_cx311111
