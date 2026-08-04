MODULE inv_cx2211111
!..use and access
use inv_core
use inv_mg2211111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b2211111, cx_f2211111
!..data
integer, parameter, public :: &
  cx_nb2211111(-1:ubound(mg2211111_nb,dim=1))=(/0,mg2211111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b2211111.f90'
include 'cx_f2211111.f90'
END MODULE inv_cx2211111
