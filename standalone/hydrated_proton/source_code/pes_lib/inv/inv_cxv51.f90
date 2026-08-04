MODULE inv_cxv51
!..use and access
use inv_core
use inv_mg51
use inv_mgv51
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b51, cxv_f51, cxv_f15, cxv_f501, cxv_f051, cxv_f105, cxv_f015, &
  cxv_f5001
!..data
integer, parameter, public :: &
  cxv_nb51(-1:ubound(mgv51_nb,dim=1))=(/0,mgv51_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b51.f90'
include 'cxv_f51.f90'
include 'cxv_f15.f90'
include 'cxv_f501.f90'
include 'cxv_f051.f90'
include 'cxv_f105.f90'
include 'cxv_f015.f90'
include 'cxv_f5001.f90'
END MODULE inv_cxv51
