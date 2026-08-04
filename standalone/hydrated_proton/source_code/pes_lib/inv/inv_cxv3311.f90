MODULE inv_cxv3311
!..use and access
use inv_core
use inv_mg3311
use inv_mgv3311
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b3311, cxv_f3311
!..data
integer, parameter, public :: &
  cxv_nb3311(-1:ubound(mgv3311_nb,dim=1))=(/0,mgv3311_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b3311.f90'
include 'cxv_f3311.f90'
END MODULE inv_cxv3311
