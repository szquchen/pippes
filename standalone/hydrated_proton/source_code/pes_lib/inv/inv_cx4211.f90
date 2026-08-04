MODULE inv_cx4211
!..use and access
use inv_core
use inv_mg4211
use inv_cxx
implicit none
private
!..procedures
public :: cx_b4211, cx_f4211
!..data
integer, parameter, public :: &
  cx_nb4211(-1:ubound(mg4211_nb,dim=1))=(/0,mg4211_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b4211.f90'
include 'cx_f4211.f90'
END MODULE inv_cx4211
