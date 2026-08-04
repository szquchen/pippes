MODULE inv_cx7111
!..use and access
use inv_core
use inv_mg7111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b7111, cx_f7111
!..data
integer, parameter, public :: &
  cx_nb7111(-1:ubound(mg7111_nb,dim=1))=(/0,mg7111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b7111.f90'
include 'cx_f7111.f90'
END MODULE inv_cx7111
