MODULE inv_cx3311
!..use and access
use inv_core
use inv_mg3311
use inv_cxx
implicit none
private
!..procedures
public :: cx_b3311, cx_f3311
!..data
integer, parameter, public :: &
  cx_nb3311(-1:ubound(mg3311_nb,dim=1))=(/0,mg3311_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b3311.f90'
include 'cx_f3311.f90'
END MODULE inv_cx3311
