MODULE inv_cxv1111111
!..use and access
use inv_core
use inv_mg1111111
use inv_mgv1111111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b1111111, cxv_f1111111
!..data
integer, parameter, public :: &
  cxv_nb1111111(-1:ubound(mgv1111111_nb,dim=1))=(/0,mgv1111111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b1111111.f90'
include 'cxv_f1111111.f90'
END MODULE inv_cxv1111111
