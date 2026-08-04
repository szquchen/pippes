MODULE inv_cxv3321
!..use and access
use inv_core
use inv_mg3321
use inv_mgv3321
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b3321, cxv_f3321
!..data
integer, parameter, public :: &
  cxv_nb3321(-1:ubound(mgv3321_nb,dim=1))=(/0,mgv3321_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b3321.f90'
include 'cxv_f3321.f90'
END MODULE inv_cxv3321
