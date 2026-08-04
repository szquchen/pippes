MODULE inv_cx3211
!..use and access
use inv_core
use inv_mg3211
use inv_cxx
implicit none
private
!..procedures
public :: cx_b3211, cx_f3211
!..data
integer, parameter, public :: &
  cx_nb3211(-1:ubound(mg3211_nb,dim=1))=(/0,mg3211_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b3211.f90'
include 'cx_f3211.f90'
END MODULE inv_cx3211
