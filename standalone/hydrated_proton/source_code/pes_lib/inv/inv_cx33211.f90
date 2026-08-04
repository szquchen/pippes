MODULE inv_cx33211
!..use and access
use inv_core
use inv_mg33211
use inv_cxx
implicit none
private
!..procedures
public :: cx_b33211, cx_f33211
!..data
integer, parameter, public :: &
  cx_nb33211(-1:ubound(mg33211_nb,dim=1))=(/0,mg33211_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b33211.f90'
include 'cx_f33211.f90'
END MODULE inv_cx33211
