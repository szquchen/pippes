MODULE inv_cx42211
!..use and access
use inv_core
use inv_mg42211
use inv_cxx
implicit none
private
!..procedures
public :: cx_b42211, cx_f42211
!..data
integer, parameter, public :: &
  cx_nb42211(-1:ubound(mg42211_nb,dim=1))=(/0,mg42211_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b42211.f90'
include 'cx_f42211.f90'
END MODULE inv_cx42211
