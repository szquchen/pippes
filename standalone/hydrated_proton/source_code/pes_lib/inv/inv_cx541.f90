MODULE inv_cx541
!..use and access
use inv_core
use inv_mg541
use inv_cxx
implicit none
private
!..procedures
public :: cx_b541, cx_f541
!..data
integer, parameter, public :: &
  cx_nb541(-1:ubound(mg541_nb,dim=1))=(/0,mg541_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b541.f90'
include 'cx_f541.f90'
END MODULE inv_cx541
