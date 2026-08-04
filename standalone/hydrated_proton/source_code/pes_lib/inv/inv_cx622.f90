MODULE inv_cx622
!..use and access
use inv_core
use inv_mg622
use inv_cxx
implicit none
private
!..procedures
public :: cx_b622, cx_f622
!..data
integer, parameter, public :: &
  cx_nb622(-1:ubound(mg622_nb,dim=1))=(/0,mg622_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b622.f90'
include 'cx_f622.f90'
END MODULE inv_cx622
