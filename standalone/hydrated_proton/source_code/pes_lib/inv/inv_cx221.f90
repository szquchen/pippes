MODULE inv_cx221
!..use and access
use inv_core
use inv_mg221
use inv_cxx
implicit none
private
!..procedures
public :: cx_b221, cx_f221, cx_f212, cx_f122, cx_f2201
!..data
integer, parameter, public :: &
  cx_nb221(-1:ubound(mg221_nb,dim=1))=(/0,mg221_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b221.f90'
include 'cx_f221.f90'
include 'cx_f212.f90'
include 'cx_f122.f90'
include 'cx_f2201.f90'
END MODULE inv_cx221
