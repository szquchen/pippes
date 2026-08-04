MODULE inv_cx32221
!..use and access
use inv_core
use inv_mg32221
use inv_cxx
implicit none
private
!..procedures
public :: cx_b32221, cx_f32221
!..data
integer, parameter, public :: &
  cx_nb32221(-1:ubound(mg32221_nb,dim=1))=(/0,mg32221_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b32221.f90'
include 'cx_f32221.f90'
END MODULE inv_cx32221
