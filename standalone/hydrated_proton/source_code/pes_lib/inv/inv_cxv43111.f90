MODULE inv_cxv43111
!..use and access
use inv_core
use inv_mg43111
use inv_mgv43111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b43111, cxv_f43111
!..data
integer, parameter, public :: &
  cxv_nb43111(-1:ubound(mgv43111_nb,dim=1))=(/0,mgv43111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b43111.f90'
include 'cxv_f43111.f90'
END MODULE inv_cxv43111
