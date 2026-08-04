MODULE inv_cxv433
!..use and access
use inv_core
use inv_mg433
use inv_mgv433
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b433, cxv_f433
!..data
integer, parameter, public :: &
  cxv_nb433(-1:ubound(mgv433_nb,dim=1))=(/0,mgv433_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b433.f90'
include 'cxv_f433.f90'
END MODULE inv_cxv433
