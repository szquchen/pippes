MODULE inv_cx5211
!..use and access
use inv_core
use inv_mg5211
use inv_cxx
implicit none
private
!..procedures
public :: cx_b5211, cx_f5211
!..data
integer, parameter, public :: &
  cx_nb5211(-1:ubound(mg5211_nb,dim=1))=(/0,mg5211_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b5211.f90'
include 'cx_f5211.f90'
END MODULE inv_cx5211
