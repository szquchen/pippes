MODULE inv_cxv211
!..use and access
use inv_core
use inv_mg211
use inv_mgv211
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b211, cxv_f211, cxv_f121, cxv_f112, &
  cxv_f2101, cxv_f1201, cxv_f2011, cxv_f0211
!..data
integer, parameter, public :: &
  cxv_nb211(-1:ubound(mgv211_nb,dim=1))=(/0,mgv211_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b211.f90'
include 'cxv_f211.f90'
include 'cxv_f121.f90'
include 'cxv_f112.f90'
include 'cxv_f2101.f90'
include 'cxv_f1201.f90'
include 'cxv_f2011.f90'
include 'cxv_f0211.f90'
END MODULE inv_cxv211
