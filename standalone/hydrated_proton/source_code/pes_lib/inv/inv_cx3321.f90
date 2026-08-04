MODULE inv_cx3321
!..use and access
use inv_core
use inv_mg3321
use inv_cxx
implicit none
private
!..procedures
public :: cx_b3321, cx_f3321
!..data
integer, parameter, public :: &
  cx_nb3321(-1:ubound(mg3321_nb,dim=1))=(/0,mg3321_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b3321.f90'
include 'cx_f3321.f90'
END MODULE inv_cx3321
