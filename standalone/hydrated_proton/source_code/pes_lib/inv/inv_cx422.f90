MODULE inv_cx422
!..use and access
use inv_core
use inv_mg422
use inv_cxx
implicit none
private
!..procedures
public :: cx_b422, cx_f422
!..data
integer, parameter, public :: &
  cx_nb422(-1:ubound(mg422_nb,dim=1))=(/0,mg422_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b422.f90'
include 'cx_f422.f90'
END MODULE inv_cx422
