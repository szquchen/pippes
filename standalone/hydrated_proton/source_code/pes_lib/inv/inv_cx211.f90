MODULE inv_cx211
!..use and access
use inv_core
use inv_mg211
use inv_cxx
implicit none
private
!..procedures
public :: cx_b211, cx_f211, cx_f121, cx_f112, &
  cx_f2101, cx_f1201, cx_f2011, cx_f0211
!..data
integer, parameter, public :: &
  cx_nb211(-1:ubound(mg211_nb,dim=1))=(/0,mg211_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b211.f90'
include 'cx_f211.f90'
include 'cx_f121.f90'
include 'cx_f112.f90'
include 'cx_f2101.f90'
include 'cx_f1201.f90'
include 'cx_f2011.f90'
include 'cx_f0211.f90'
END MODULE inv_cx211
