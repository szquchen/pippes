MODULE inv_cxv221111
!..use and access
use inv_core
use inv_mg221111
use inv_mgv221111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b221111, cxv_f221111
!..data
integer, parameter, public :: &
  cxv_nb221111(-1:ubound(mgv221111_nb,dim=1))=(/0,mgv221111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b221111.f90'
include 'cxv_f221111.f90'
END MODULE inv_cxv221111
