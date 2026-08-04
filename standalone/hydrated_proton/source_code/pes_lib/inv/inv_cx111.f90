MODULE inv_cx111
!..use and access
use inv_core
use inv_mg111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b111, cx_f111, cx_f1101, cx_f1011, cx_f0111
!..data
integer, parameter, public :: &
  cx_nb111(-1:ubound(mg111_nb,dim=1))=(/0,mg111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b111.f90'
include 'cx_f111.f90'
include 'cx_f1101.f90'
include 'cx_f1011.f90'
include 'cx_f0111.f90'
END MODULE inv_cx111
