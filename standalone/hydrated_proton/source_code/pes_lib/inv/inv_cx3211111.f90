MODULE inv_cx3211111
!..use and access
use inv_core
use inv_mg3211111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b3211111, cx_f3211111
!..data
integer, parameter, public :: &
  cx_nb3211111(-1:ubound(mg3211111_nb,dim=1))=(/0,mg3211111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b3211111.f90'
include 'cx_f3211111.f90'
END MODULE inv_cx3211111
