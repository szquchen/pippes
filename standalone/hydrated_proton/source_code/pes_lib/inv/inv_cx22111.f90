MODULE inv_cx22111
!..use and access
use inv_core
use inv_mg22111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b22111, cx_f22111
!..data
integer, parameter, public :: &
  cx_nb22111(-1:ubound(mg22111_nb,dim=1))=(/0,mg22111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b22111.f90'
include 'cx_f22111.f90'
END MODULE inv_cx22111
