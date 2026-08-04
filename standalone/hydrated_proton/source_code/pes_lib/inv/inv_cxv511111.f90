MODULE inv_cxv511111
!..use and access
use inv_core
use inv_mg511111
use inv_mgv511111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b511111, cxv_f511111
!..data
integer, parameter, public :: &
  cxv_nb511111(-1:ubound(mgv511111_nb,dim=1))=(/0,mgv511111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b511111.f90'
include 'cxv_f511111.f90'
END MODULE inv_cxv511111
