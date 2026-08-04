MODULE inv_cx22211
!..use and access
use inv_core
use inv_mg22211
use inv_cxx
implicit none
private
!..procedures
public :: cx_b22211, cx_f22211
!..data
integer, parameter, public :: &
  cx_nb22211(-1:ubound(mg22211_nb,dim=1))=(/0,mg22211_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b22211.f90'
include 'cx_f22211.f90'
END MODULE inv_cx22211
