MODULE inv_cx6
!..use and access
use inv_core
use inv_mg6
use inv_cxx
implicit none
private
!..procedures
public :: cx_b6, cx_f6, cx_f06, cx_f006
!..data
integer, parameter, public :: &
  cx_nb6(-1:ubound(mg6_nb,dim=1))=(/0,mg6_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b6.f90'
include 'cx_f6.f90'
include 'cx_f06.f90'
include 'cx_f006.f90'
END MODULE inv_cx6
