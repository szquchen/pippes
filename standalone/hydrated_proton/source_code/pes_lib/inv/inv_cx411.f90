MODULE inv_cx411
!..use and access
use inv_core
use inv_mg411
use inv_cxx
implicit none
private
!..procedures
public :: cx_b411, cx_f411, cx_f141, cx_f114, cx_f4101, cx_f4011
!..data
integer, parameter, public :: &
  cx_nb411(-1:ubound(mg411_nb,dim=1))=(/0,mg411_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b411.f90'
include 'cx_f411.f90'
include 'cx_f141.f90'
include 'cx_f114.f90'
include 'cx_f4101.f90'
include 'cx_f4011.f90'
END MODULE inv_cx411
