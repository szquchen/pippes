MODULE inv_cx3111111
!..use and access
use inv_core
use inv_mg3111111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b3111111, cx_f3111111
!..data
integer, parameter, public :: &
  cx_nb3111111(-1:ubound(mg3111111_nb,dim=1))=(/0,mg3111111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b3111111.f90'
include 'cx_f3111111.f90'
END MODULE inv_cx3111111
