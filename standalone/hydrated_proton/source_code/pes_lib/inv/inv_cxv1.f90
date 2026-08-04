MODULE inv_cxv1
!..use and access
use inv_core
use inv_mg1
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b1, cxv_f1, cxv_f01, cxv_f001, cxv_f0001
!..data
integer, parameter, public :: &
  cxv_nb1=1
!..procedures
CONTAINS
include 'pure_sum.f90'
include 'cxv_b1.f90'
include 'cxv_f1.f90'
include 'cxv_f01.f90'
include 'cxv_f001.f90'
include 'cxv_f0001.f90'
END MODULE inv_cxv1
