MODULE inv_cx6211
!..use and access
use inv_core
use inv_mg6211
use inv_cxx
implicit none
private
!..procedures
public :: cx_b6211, cx_f6211
!..data
integer, parameter, public :: &
  cx_nb6211(-1:ubound(mg6211_nb,dim=1))=(/0,mg6211_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b6211.f90'
include 'cx_f6211.f90'
END MODULE inv_cx6211
