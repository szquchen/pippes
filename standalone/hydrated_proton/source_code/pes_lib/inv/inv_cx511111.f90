MODULE inv_cx511111
!..use and access
use inv_core
use inv_mg511111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b511111, cx_f511111
!..data
integer, parameter, public :: &
  cx_nb511111(-1:ubound(mg511111_nb,dim=1))=(/0,mg511111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b511111.f90'
include 'cx_f511111.f90'
END MODULE inv_cx511111
