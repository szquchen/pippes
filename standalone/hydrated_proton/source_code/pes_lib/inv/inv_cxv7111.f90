MODULE inv_cxv7111
!..use and access
use inv_core
use inv_mg7111
use inv_mgv7111
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b7111, cxv_f7111
!..data
integer, parameter, public :: &
  cxv_nb7111(-1:ubound(mgv7111_nb,dim=1))=(/0,mgv7111_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b7111.f90'
include 'cxv_f7111.f90'
END MODULE inv_cxv7111
