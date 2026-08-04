MODULE inv_cx41
!..use and access
use inv_core
use inv_mg41
use inv_cxx
implicit none
private
!..procedures
public :: cx_b41, cx_f41, cx_f14, cx_f401, cx_f041, cx_f104, cx_f014, &
  cx_f4001
!..data
integer, parameter, public :: &
  cx_nb41(-1:ubound(mg41_nb,dim=1))=(/0,mg41_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b41.f90'
include 'cx_f41.f90'
include 'cx_f14.f90'
include 'cx_f401.f90'
include 'cx_f041.f90'
include 'cx_f104.f90'
include 'cx_f014.f90'
include 'cx_f4001.f90'
END MODULE inv_cx41
