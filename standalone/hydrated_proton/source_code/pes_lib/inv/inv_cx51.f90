MODULE inv_cx51
!..use and access
use inv_core
use inv_mg51
use inv_cxx
implicit none
private
!..procedures
public :: cx_b51, cx_f51, cx_f15, cx_f501, cx_f051, cx_f105, cx_f015, &
  cx_f5001
!..data
integer, parameter, public :: &
  cx_nb51(-1:ubound(mg51_nb,dim=1))=(/0,mg51_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b51.f90'
include 'cx_f51.f90'
include 'cx_f15.f90'
include 'cx_f501.f90'
include 'cx_f051.f90'
include 'cx_f105.f90'
include 'cx_f015.f90'
include 'cx_f5001.f90'
END MODULE inv_cx51
