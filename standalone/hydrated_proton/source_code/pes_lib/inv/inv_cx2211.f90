MODULE inv_cx2211
!..use and access
use inv_core
use inv_mg2211
use inv_cxx
implicit none
private
!..procedures
public :: cx_b2211, cx_f2211
!..data
integer, parameter, public :: &
  cx_nb2211(-1:ubound(mg2211_nb,dim=1))=(/0,mg2211_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b2211.f90'
include 'cx_f2211.f90'
END MODULE inv_cx2211
