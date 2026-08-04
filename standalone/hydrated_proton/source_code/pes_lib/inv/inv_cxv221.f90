MODULE inv_cxv221
!..use and access
use inv_core
use inv_mg221
use inv_mgv221
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b221, cxv_f221, cxv_f212, cxv_f122, cxv_f2201
!..data
integer, parameter, public :: &
  cxv_nb221(-1:ubound(mgv221_nb,dim=1))=(/0,mgv221_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b221.f90'
include 'cxv_f221.f90'
include 'cxv_f212.f90'
include 'cxv_f122.f90'
include 'cxv_f2201.f90'
END MODULE inv_cxv221
