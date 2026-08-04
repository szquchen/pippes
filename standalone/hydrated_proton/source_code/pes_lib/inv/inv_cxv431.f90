MODULE inv_cxv431
!..use and access
use inv_core
use inv_mg431
use inv_mgv431
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b431, cxv_f431
!..data
integer, parameter, public :: &
  cxv_nb431(-1:ubound(mgv431_nb,dim=1))=(/0,mgv431_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b431.f90'
include 'cxv_f431.f90'
END MODULE inv_cxv431
