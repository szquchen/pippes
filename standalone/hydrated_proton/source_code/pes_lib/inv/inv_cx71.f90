MODULE inv_cx71
!..use and access
use inv_core
use inv_mg71
use inv_cxx
implicit none
private
!..procedures
public :: cx_b71, cx_f71
!..data
integer, parameter, public :: &
  cx_nb71(-1:ubound(mg71_nb,dim=1))=(/0,mg71_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b71.f90'
include 'cx_f71.f90'
END MODULE inv_cx71
