MODULE inv_cx442
!..use and access
use inv_core
use inv_mg442
use inv_cxx
implicit none
private
!..procedures
public :: cx_b442, cx_f442
!..data
integer, parameter, public :: &
  cx_nb442(-1:ubound(mg442_nb,dim=1))=(/0,mg442_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b442.f90'
include 'cx_f442.f90'
END MODULE inv_cx442
