MODULE inv_cx511
!..use and access
use inv_core
use inv_mg511
use inv_cxx
implicit none
private
!..procedures
public :: cx_b511, cx_f511, cx_f5101, cx_f5011
!..data
integer, parameter, public :: &
  cx_nb511(-1:ubound(mg511_nb,dim=1))=(/0,mg511_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b511.f90'
include 'cx_f511.f90'
include 'cx_f5101.f90'
include 'cx_f5011.f90'
END MODULE inv_cx511
